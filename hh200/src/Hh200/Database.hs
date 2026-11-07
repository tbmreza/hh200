{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Hh200.Database
    where

import Debug.Trace

import           GHC.Generics (Generic)
import           Control.Exception (try, SomeException, displayException)
import           Control.Monad (forM)
import           Data.Aeson (ToJSON (..), object, (.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BSC
import           Data.Int (Int64)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import           Database.SQLite.Simple (query, Only (..), Connection, ToRow (..), FromRow (..), toRow, fromRow, execute_, execute, close, open, field, query_, lastInsertRowId)
import           Network.HTTP.Types.Header (HeaderName)
import           System.Directory (XdgDirectory (XdgData), getXdgDirectory, createDirectoryIfMissing)
import           System.Environment (lookupEnv)
import           System.Exit (exitSuccess)
import           System.FilePath ((<.>), (</>))


data HtmlReportData = HtmlReportData
  { reportId   :: Int
  , reportHtml :: Text
  } deriving (Show, Generic)

instance ToJSON HtmlReportData

joinLatestReport :: Connection -> IO (Either Text (Maybe HtmlReportData))
joinLatestReport conn = do
    result <- try $
        execute conn
            ""
            ()

    pure $ case result of
        Left (e :: SomeException) ->
            Left (Text.pack (displayException e))
        Right _ ->
            Right Nothing

newtype RunId = RunId Int

data RREndTime =
    ETStillRunning
  -- | ETHasEnded !Int64
  | ETHasEnded Int64
    deriving (Show, Eq)

data RunRow = RunRow
  { runId            :: Int64
  , runName          :: Text
  , runScriptPath    :: Text
  , runStartedAt     :: Int64
  , runEndedAt       :: RREndTime
  , runStatus        :: Text
  , runConcurrency   :: Int
  , runRateLimit     :: Double
  , runControlSocket :: Text
  }

instance ToRow RunRow where
    toRow (RunRow rid n sp sa ea s c rl cs) = toRow (rid, n, sp, sa, endedAtVal, s, c, rl, cs)
      where
        endedAtVal = case ea of ETStillRunning -> Nothing; ETHasEnded v -> Just v

instance FromRow RunRow where
    fromRow = RunRow <$> field
                     <*> field
                     <*> field
                     <*> field
                     <*> (maybe ETStillRunning ETHasEnded <$> field)
                     <*> field
                     <*> field
                     <*> field
                     <*> field

instance ToJSON RunRow where
    toJSON (RunRow rid n sp sa ea s c rl cs) = object
      [ "id" .= rid
      , "name" .= n
      , "script_path" .= sp
      , "started_at" .= sa
      , "ended_at" .= case ea of ETStillRunning -> Nothing; ETHasEnded v -> Just v
      , "status" .= s
      , "concurrency" .= c
      , "rate_limit" .= rl
      , "control_socket" .= cs
      ]

initDb :: IO Connection
initDb = do
    mPath <- lookupEnv "HH200_SQLITE"
    fp <- case mPath of
        Just fp -> pure fp
        Nothing -> do
            dir <- getXdgDirectory XdgData "hh200"
            createDirectoryIfMissing True dir
            pure (dir </> "hh200.sqlite")
    open fp

closeDb :: Connection -> IO ()
closeDb = close

updateRunStatus :: Connection -> Int64 -> Text -> IO (Either Text ())
updateRunStatus conn runId status = do
    result <- try $
        execute conn
            "UPDATE runs SET status = ? WHERE id = ?"
            (status, runId)

    case result of
        Left (e :: SomeException) ->
            pure $ Left (Text.pack (displayException e))
        Right _ ->
            pure $ Right ()

getRunById :: Connection -> Int64 -> IO (Either Text RunRow)
getRunById conn rid = do
    result <- try $
        query conn
            "SELECT id, name, script_path, started_at, ended_at, status, concurrency, rate_limit, control_socket \
            \FROM runs WHERE id = ?"
            (Only rid)

    case result of
        Left (e :: SomeException) ->
            pure $ Left (Text.pack (displayException e))
        Right [] ->
            pure $ Left "Run not found"
        Right (row : _) ->
            pure $ Right row

getLatestRunningRun :: Connection -> IO (Either Text (Maybe RunRow))
getLatestRunningRun conn = do
    result <- try $
        query_ conn
            "SELECT id, name, script_path, started_at, ended_at, status, concurrency, rate_limit, control_socket \
            \FROM runs WHERE status = 'running' ORDER BY started_at DESC LIMIT 1"

    case result of
        Left (e :: SomeException) ->
            pure $ Left (Text.pack (displayException e))
        Right [] ->
            pure $ Right Nothing
        Right (row : _) ->
            pure $ Right (Just row)


-- class DbInsert -- ??

insertRun :: Connection -> RunRow -> IO (Either Text Int64)
insertRun conn rr = do
    result <- try $ do
        execute conn
            "INSERT INTO runs (name, script_path, started_at, ended_at, status, concurrency, rate_limit, control_socket) VALUES (?, ?, unixepoch('now'), ?, ?, ?, ?, ?)"
            ( runName rr
            , runScriptPath rr
            , endedAtSql (runEndedAt rr)
            , runStatus rr
            , runConcurrency rr
            , runRateLimit rr
            , runControlSocket rr
            )
        lastInsertRowId conn

    case result of
        Left (e :: SomeException) ->
            pure $ Left (Text.pack (displayException e))
        Right rid ->
            pure $ Right rid

    where
    endedAtSql ETStillRunning = Nothing
    endedAtSql (ETHasEnded v) = Just v

listRuns :: Connection -> IO [RunRow]
listRuns conn = query_ conn "SELECT id, name, script_path, started_at, ended_at, status, concurrency, rate_limit, control_socket FROM runs ORDER BY started_at DESC"

--------------------------------------------------------------------------------
-- Request persistence (requests / request_headers / request_bodies)
--------------------------------------------------------------------------------

-- | A @requests@ row. @requestId@ is @0@ on insert (the id is auto-assigned)
-- and the real id when read back.
data RequestRow = RequestRow
  { requestId         :: Int64
  , requestRunId      :: Int64
  , requestSeq        :: Int
  , requestSentAt     :: Int64       -- ^ unix epoch seconds, like @runs.started_at@
  , requestDurationMs :: Maybe Double -- ^ NULL on error
  , requestMethod     :: Text
  , requestUrl        :: Text
  , requestStatus     :: Maybe Int   -- ^ NULL on error
  , requestError      :: Maybe Text
  , requestBytesIn    :: Maybe Int
  , requestBytesOut   :: Maybe Int
  , requestWorkerId   :: Maybe Int
  } deriving (Show, Eq)

instance ToRow RequestRow where
    toRow (RequestRow _id rid sq sa dm m u st e bi bo wi) =
        toRow (rid, sq, sa, dm, m, u) ++ toRow (st, e, bi, bo, wi)

instance FromRow RequestRow where
    fromRow = RequestRow <$> field <*> field <*> field <*> field <*> field
                         <*> field <*> field <*> field <*> field <*> field
                         <*> field <*> field

-- | A row for INSERT into @request_headers@ (@request_id@ is supplied at insert time).
data RequestHeaderRow = RequestHeaderRow
  { requestHeaderDirection :: Text -- ^ "request" | "response"
  , requestHeaderName      :: Text
  , requestHeaderValue     :: Text
  } deriving (Show, Eq)

-- | A row for INSERT into @request_bodies@.
data RequestBodyRow = RequestBodyRow
  { requestBodyDirection :: Text             -- ^ "request" | "response"
  , requestBodyContent   :: Maybe BS.ByteString -- ^ NULL when body unavailable (e.g. streamed)
  , requestBodyTruncated :: Bool
  } deriving (Show, Eq)

-- | A complete request to persist: one @requests@ row plus its headers/bodies.
data CapturedRequest = CapturedRequest
  { capturedRow     :: RequestRow
  , capturedHeaders :: [RequestHeaderRow]
  , capturedBodies  :: [RequestBodyRow]
  } deriving (Show, Eq)

insertRequest :: Connection -> RequestRow -> IO (Either Text Int64)
insertRequest conn rr = do
    result <- try $ do
        execute conn
            "INSERT INTO requests (run_id, seq, sent_at, duration_ms, method, url, status_code, error, bytes_in, bytes_out, worker_id) \
            \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
            rr
        lastInsertRowId conn
    case result of
        Left (e :: SomeException) -> pure (Left (Text.pack (displayException e)))
        Right rid -> pure (Right rid)

insertRequestHeader :: Connection -> Int64 -> RequestHeaderRow -> IO (Either Text ())
insertRequestHeader conn rid (RequestHeaderRow dir name value) = do
    result <- (try $ execute conn
        "INSERT INTO request_headers (request_id, direction, name, value) VALUES (?, ?, ?, ?)"
        (rid, dir, name, value)) :: IO (Either SomeException ())
    pure $ either (Left . Text.pack . displayException) (const (Right ())) result

insertRequestBody :: Connection -> Int64 -> RequestBodyRow -> IO (Either Text ())
insertRequestBody conn rid (RequestBodyRow dir mContent truncated) = do
    result <- (try $ execute conn
        "INSERT INTO request_bodies (request_id, direction, content, truncated) VALUES (?, ?, ?, ?)"
        (rid, dir, mContent, truncated)) :: IO (Either SomeException ())
    pure $ either (Left . Text.pack . displayException) (const (Right ())) result

-- | Insert a request and all of its headers/bodies under one request id.
insertCapturedRequest :: Connection -> CapturedRequest -> IO (Either Text ())
insertCapturedRequest conn (CapturedRequest rr hs bs) = do
    mRid <- insertRequest conn rr
    case mRid of
        Left e -> pure (Left e)
        Right rid -> do
            hsResults <- mapM (insertRequestHeader conn rid) hs
            case sequence hsResults of
                Left e -> pure (Left e)
                Right _ -> do
                    bsResults <- mapM (insertRequestBody conn rid) bs
                    case sequence bsResults of
                        Left e -> pure (Left e)
                        Right _ -> pure (Right ())

--------------------------------------------------------------------------------
-- Request retrieval (for GET /api/runs/:runId)
--------------------------------------------------------------------------------

-- | A @request_headers@ row read back from SQLite (with its id).
data RequestHeaderFull = RequestHeaderFull
  { requestHeaderFullId        :: Int64
  , requestHeaderFullDirection :: Text
  , requestHeaderFullName      :: Text
  , requestHeaderFullValue     :: Text
  } deriving (Show, Eq)

instance ToJSON RequestHeaderFull where
    toJSON (RequestHeaderFull hid dir name value) = object
        [ "id" .= hid
        , "direction" .= dir
        , "name" .= name
        , "value" .= value
        ]

-- | A @request_bodies@ row read back from SQLite (with its id).
data RequestBodyFull = RequestBodyFull
  { requestBodyFullId        :: Int64
  , requestBodyFullDirection :: Text
  , requestBodyFullContent   :: Maybe BS.ByteString
  , requestBodyFullTruncated :: Bool
  } deriving (Show, Eq)

instance ToJSON RequestBodyFull where
    toJSON (RequestBodyFull bid dir mContent truncated) = object
        [ "id" .= bid
        , "direction" .= dir
        , "content" .= fmap (TE.decodeUtf8 . B64.encode) mContent
        , "truncated" .= truncated
        ]

-- | A request plus its nested headers/bodies, shaped like the frontend's
-- @Request@ in @live/src/lib/types.ts@.
data RequestDetail = RequestDetail
  { detailRow     :: RequestRow
  , detailHeaders :: [RequestHeaderFull]
  , detailBodies  :: [RequestBodyFull]
  } deriving (Show, Eq)

instance ToJSON RequestDetail where
    toJSON (RequestDetail rr hs bs) = object
        [ "id" .= requestId rr
        , "seq" .= requestSeq rr
        , "sent_at" .= requestSentAt rr
        , "duration_ms" .= requestDurationMs rr
        , "method" .= requestMethod rr
        , "url" .= requestUrl rr
        , "status_code" .= requestStatus rr
        , "error" .= requestError rr
        , "bytes_in" .= requestBytesIn rr
        , "bytes_out" .= requestBytesOut rr
        , "worker_id" .= requestWorkerId rr
        , "headers" .= hs
        , "bodies" .= bs
        ]

-- | All requests for a run, ordered by @seq@, with nested headers/bodies.
listRequestsByRun :: Connection -> Int64 -> IO (Either Text [RequestDetail])
listRequestsByRun conn runId = do
    result <- try $ do
        rows <- query conn
            "SELECT id, run_id, seq, sent_at, duration_ms, method, url, status_code, error, bytes_in, bytes_out, worker_id \
            \FROM requests WHERE run_id = ? ORDER BY seq, id"
            (Only runId) :: IO [RequestRow]
        forM rows $ \rr -> do
            hs <- query conn
                "SELECT id, direction, name, value FROM request_headers WHERE request_id = ? ORDER BY id"
                (Only (requestId rr))
            bs <- query conn
                "SELECT id, direction, content, truncated FROM request_bodies WHERE request_id = ? ORDER BY id"
                (Only (requestId rr))
            pure $ RequestDetail rr
                [ RequestHeaderFull h d n v | (h, d, n, v) <- hs ]
                [ RequestBodyFull b d c t | (b, d, c, t) <- bs ]
    case result of
        Left (e :: SomeException) -> pure (Left (Text.pack (displayException e)))
        Right details -> pure (Right details)
