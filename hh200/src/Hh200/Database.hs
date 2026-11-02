{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Hh200.Database
    where

import Debug.Trace

import           GHC.Generics (Generic)
import           Control.Exception (try, SomeException, displayException)
import           Data.Aeson (ToJSON (..), object, (.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.Int (Int64)
import           Data.Text (Text)
import qualified Data.Text as Text
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
