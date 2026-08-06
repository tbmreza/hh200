{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hh200.Database
    where

import Debug.Trace

import           Control.Exception (try, SomeException, displayException)
import           Data.Aeson (ToJSON (..), object, (.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.Int (Int64)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Csv as Csv
import           Database.SQLite.Simple (query, Only (..), Connection, ToRow (..), FromRow (..), toRow, fromRow, execute_, execute, close, open, field, query_, lastInsertRowId)
import           Network.HTTP.Types.Header (HeaderName)
import           System.Directory (XdgDirectory (XdgData), getXdgDirectory)
import           System.Environment (lookupEnv)
import           System.Exit (exitSuccess)
import           System.FilePath ((<.>), (</>))


newtype RunId = RunId Int

data RREndTime =
    ETStillRunning
  -- | ETHasEnded !Int64
  | ETHasEnded Int64
    deriving (Show, Eq)

-- (auto) ebpf field will be added here
-- ??: plan how metrics will be filtered
data MetricRow = MetricRow
  { metricTcpRetransmits :: Int
  -- { metricRunId       :: Int64
  -- , metricWorkerId    :: Int
  -- , metricTimestampMs :: Int64
  -- , metricLatencyMs   :: Double
  -- , metricStatusCode  :: Int
  -- , metricError       :: Maybe Text   -- Nothing on success; timeout/conn-refused/etc. on failure
  } deriving (Show, Eq)

data RunRow = RunRow
  { runName          :: Text
  , runScriptPath    :: Text
  , runStartedAt     :: Int64
  , runEndedAt       :: RREndTime
  , runStatus        :: Text
  , runConcurrency   :: Int
  , runRateLimit     :: Double
  , runControlSocket :: Text
  }

-- ??: as prisma call, or if needed at all
migrateSchema :: Connection -> IO (Either Text ())
migrateSchema = undefined

getMetricsForRun :: Connection -> Int64 -> IO [MetricRow]
getMetricsForRun = undefined

instance ToRow RunRow where
    toRow (RunRow n sp sa ea s c rl cs) = toRow (n, sp, sa, endedAtVal, s, c, rl, cs)
      where
        endedAtVal = case ea of ETStillRunning -> Nothing; ETHasEnded v -> Just v

instance FromRow RunRow where
    fromRow = RunRow <$> field
                     <*> field
                     <*> field
                     <*> (maybe ETStillRunning ETHasEnded <$> field)
                     <*> field
                     <*> field
                     <*> field
                     <*> field

instance ToJSON RunRow where
    toJSON (RunRow n sp sa ea s c rl cs) = object
      [ "name" .= n
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
    case mPath of
        Just fp -> open (trace ("kmPath=" ++ fp) fp)
        Nothing -> do
            dir <- getXdgDirectory XdgData "hh200"
            putStrLn dir
            exitSuccess

closeDb :: Connection -> IO ()
closeDb = close

-- insertRun :: Connection -> RunRow -> IO (Maybe Int64)
-- insertRun conn rr = do
--     result <- try $ do
--         execute conn
--                 "INSERT INTO runs (name, script_path, started_at, ended_at, status, concurrency, rate_limit, control_socket) VALUES (?, ?, unixepoch('now'), ?, ?, ?, ?, ?)"
--                 (runName rr, runScriptPath rr, endedAtSql (runEndedAt rr), runStatus rr, runConcurrency rr, runRateLimit rr, runControlSocket rr)
--         lastInsertRowId conn
--     case result of
--         Left (_ :: SomeException) -> pure Nothing
--         Right rid -> pure (Just rid)
--   where
--     endedAtSql ETStillRunning = Nothing
--     endedAtSql (ETHasEnded v) = Just v

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


insertMetric :: Connection -> MetricRow -> IO (Either Text ())
insertMetric = undefined
-- insertMetric conn mr = do
--     result <- try $
--         execute conn
--             "INSERT INTO metrics \
--             \(run_id, worker_id, timestamp_ms, latency_ms, status_code, error) \
--             \VALUES (?, ?, ?, ?, ?, ?)"
--             ( metricRunId mr
--             , metricWorkerId mr
--             , metricTimestampMs mr
--             , metricLatencyMs mr
--             , metricStatusCode mr
--             , metricError mr
--             )
--
--     case result of
--         Left (e :: SomeException) ->
--             pure $ Left (Text.pack (displayException e))
--         Right () ->
--             pure $ Right ()

data RunMetric = RunMetric
  { csvRunName :: Text
  , csvMetricTcp :: Int
  } deriving (Show)
type StatsHistory = [RunMetric]

instance Csv.ToNamedRecord RunMetric where
    toNamedRecord (RunMetric runName metricTcp) = undefined
-- instance Csv.ToNamedRecord UserPost where
--     toNamedRecord (UserPost name title) =
--         Csv.namedRecord
--             [ "user_name" Csv..= name
--             , "post_title" Csv..= title
--             ]
--
-- instance Csv.DefaultOrdered UserPost where  -- ??: when use case comes up
--     headerOrder _ =
--         Csv.header
--             [ "user_name"
--             , "post_title"
--             ]

-- runs-metrics joined to feed to report serializer.
-- queryStatsHistory :: Connection -> ... -> IO (Either Text StatsHistory)
queryStatsHistory :: Connection -> IO (Either Text StatsHistory)
queryStatsHistory conn = do
    pure (Right [])

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
listRuns conn = query_ conn "SELECT name, script_path, started_at, ended_at, status, concurrency, rate_limit, control_socket FROM runs ORDER BY started_at DESC"
