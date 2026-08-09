{-# LANGUAGE OverloadedStrings, NumericUnderscores #-}

module DatabaseSpec (spec) where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Options
import           Test.Tasty.Runners

import           Control.Exception (bracket)
import qualified Data.ByteString as BS
import           Data.Int (Int64)
import qualified Data.Text as Text
import           Data.Text (Text)
import           System.Directory (copyFile, createDirectoryIfMissing, doesFileExist, removeDirectoryRecursive, getCurrentDirectory)
import           System.Environment (lookupEnv, setEnv)
import           System.FilePath ((</>), takeDirectory)
import           System.IO.Temp (withSystemTempDirectory)
-- import           Database.SQLite.Simple (Connection, ToRow (..), FromRow (..), toRow, fromRow, execute_, execute, close, open, field, query_, lastInsertRowId)
import           Database.SQLite.Simple (Connection, query_, Only (..), open, close, execute_)

import           Hh200.Database

mkRun :: Text -> RunRow
mkRun name = RunRow
  { runId            = 0
  , runName          = name
  , runScriptPath    = "scripts/example.hhs"
  , runStartedAt     = 1_700_000_000
  , runEndedAt       = ETStillRunning
  , runStatus        = "running"
  , runConcurrency   = 10
  , runRateLimit     = 50.0
  , runControlSocket = "/tmp/hh200-control.sock"
  }

mkMetric :: Int64 -> Int -> MetricRow
mkMetric rid workerId = MetricRow
  { 
  }
  -- { metricRunId       = rid
  -- , metricWorkerId    = workerId
  -- , metricTimestampMs = 1_700_000_000_000
  -- , metricLatencyMs   = 12.5
  -- , metricStatusCode  = 200
  -- , metricError       = Nothing
  -- }


-- GOAL insert to main.runs every new stack run
-- ??: downloading a run report joins metrics on runId

prismaProjectDir :: IO FilePath
prismaProjectDir = do
    here <- getCurrentDirectory
    pure (takeDirectory here </> "live")


fixtureDbPathEnvVar :: String
fixtureDbPathEnvVar = "HH200_TEST_FIXTURE_DB"

appDbPath :: FilePath -> FilePath
appDbPath projDir = projDir </> "prisma" </> "app.db"

withPreMigratedDb :: (Connection -> IO ()) -> IO ()
withPreMigratedDb action = do
    projDir <- prismaProjectDir
    bracket (openPrepared (appDbPath projDir))
            close
            action
    where
    openPrepared path = do
        conn <- open path
        execute_ conn "PRAGMA busy_timeout = 5000;"
        execute_ conn "PRAGMA journal_mode = WAL;"
        execute_ conn "PRAGMA synchronous = NORMAL;"
        pure conn

synchronously = localOption (NumThreads 1)
spec :: TestTree
spec = synchronously $ testGroup "sqlite"
  -- [ testGroup "connection lifecycle"
  --     -- ??: testCase is ineffective *and* jumped over with withPreMigratedDb
  --     [ testCase "init and close db" $ withRawDb $ \_conn dbPath -> do
  --         exists <- doesFileExist dbPath
  --         assertBool "database file should exist after initDb" exists
  --     ]

  -- [ synchronously $ testGroup "run rows"
  --     [ testCase "insertRun returns a usable rowid" $ withPreMigratedDb $ \conn -> do
  --         rid <- mustRight "" (insertRun conn (mkRun "first-run"))
  --         assertBool "rowid should be positive" (rid > 0)
  --     ]
  --
  -- , synchronously $ testGroup "run status transitions"
  --     [ testCase "updateRunStatus changes status without touching ended_at" $ withPreMigratedDb $ \conn -> do
  --         rid <- mustRight "rid" (insertRun conn (mkRun "pausable"))
  --         _   <- mustRight "paused" (updateRunStatus conn rid "paused")
  --         -- rr  <- mustJust "run should still exist" (getRunById conn rid)
  --         rr  <- mustRight "rr" (getRunById conn rid)
  --         runStatus rr   @?= "paused"
  --         -- runEndedAt rr  @?= RRStillRunning
  --         runEndedAt rr  @?= ETStillRunning
  --     ]

  [ testGroup "metrics"
      [ testCase "insertMetric then getMetricsForRun round-trips one row" $ withPreMigratedDb $ \conn -> do
          rid  <- mustRight (insertRun conn (mkRun "metric-run"))
          _    <- mustRight (insertMetric conn (mkMetric rid 0))
          rows <- getMetricsForRun conn rid
          case rows of
            [m] -> do
              metricWorkerId m   @?= 0
              metricStatusCode m @?= 200
            _ -> assertFailure ("expected exactly one metric row, got " ++ show (length rows))
      ]

  ]

withRawDb :: (Connection -> FilePath -> IO ()) -> IO ()
withRawDb action =
  withSystemTempDirectory "hh200-test-db" $ \tmpDir -> do
    let dbPath = tmpDir </> "test.db"
    setEnv "HH200_SQLITE" dbPath
    conn <- initDb
    action conn dbPath
    closeDb conn


--------------------------------------------------------------------------------
-- More lib than app code
--------------------------------------------------------------------------------
mustRight :: String -> IO (Either Text a) -> IO a
mustRight caller action = do
  result <- action
  case result of
    Left err -> assertFailure (caller ++ "expected Right, got Left: " ++ Text.unpack err)
    Right a  -> pure a

mustJust :: String -> IO (Maybe a) -> IO a
mustJust msg action = do
  result <- action
  case result of
    Nothing -> assertFailure msg
    Just a  -> pure a
