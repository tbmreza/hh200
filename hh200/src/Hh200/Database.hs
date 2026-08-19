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
import           System.Directory (XdgDirectory (XdgData), getXdgDirectory, createDirectoryIfMissing)
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
data MetricWindowRow = MetricWindowRow
  { mwRps :: Int
  -- { metricTcpRetransmits :: Int
  -- { metricRunId       :: Int64
  -- , metricWorkerId    :: Int
  -- , metricTimestampMs :: Int64
  -- , metricLatencyMs   :: Double
  -- , metricStatusCode  :: Int
  -- , metricError       :: Maybe Text   -- Nothing on success; timeout/conn-refused/etc. on failure
  } deriving (Show, Eq)

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

-- migrateSchema :: Connection -> IO (Either Text ())
-- migrateSchema = undefined

getMetricsForRun :: Connection -> Int64 -> IO [MetricWindowRow]
getMetricsForRun = undefined

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


insertMetric :: Connection -> MetricWindowRow -> IO (Either Text ())
insertMetric = undefined
-- insertMetricWindow conn mr = do
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
  -- , csvMetricTcp :: Int
  , csvMwRps :: Int
  } deriving (Show)
type StatsHistory = [RunMetric]

instance Csv.ToNamedRecord RunMetric where
    toNamedRecord (RunMetric r0 r1) =
        Csv.namedRecord
            [ "run_name" Csv..= r0
            , "metric_window_rps" Csv..= r1
            ]

instance Csv.DefaultOrdered RunMetric where
    headerOrder _ =
        Csv.header
            [ "run_name"
            , "metric_window_rps"
            ]

-- runs-metrics joined to feed to report serializer.
queryStatsHistory :: Connection -> Int -> IO (Either Text StatsHistory)
queryStatsHistory conn runId = do
    result <- try $
        (query conn
            "SELECT r.name, m.id \
            \FROM metric_windows m JOIN runs r ON r.id = m.run_id \
            \WHERE m.run_id = ? ORDER BY m.id"
            (Only runId) :: IO [(Text, Int64)])

    case result of
        Left (e :: SomeException) ->
            pure $ Left (Text.pack (displayException e))
        Right rows ->
            pure $ Right [ RunMetric name (fromIntegral mId) | (name, mId) <- rows ]

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
