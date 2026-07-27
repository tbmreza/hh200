{-# LANGUAGE OverloadedStrings, NumericUnderscores #-}

module DatabaseSpec (spec) where

import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Data.ByteString as BS
import           Data.Int (Int64)
import           Data.Text (Text)
import           System.Directory (createDirectoryIfMissing, doesFileExist, removeDirectoryRecursive)
import           System.Environment (setEnv)
import           System.FilePath ((</>))
import           System.IO.Temp (withSystemTempDirectory)
-- import           Database.SQLite.Simple (Connection, ToRow (..), FromRow (..), toRow, fromRow, execute_, execute, close, open, field, query_, lastInsertRowId)
import           Database.SQLite.Simple (Connection)

import           Hh200.Database

-- mkRun :: Text -> RunRow
-- mkRun name = RunRow
--   { runName          = name
--   , runScriptPath    = "scripts/example.hhs"
--   , runStartedAt     = 1_700_000_000
--   , runEndedAt       = RRStillRunning
--   , runStatus        = "running"
--   , runConcurrency   = 10
--   , runRateLimit     = 50.0
--   , runControlSocket = "/tmp/hh200-control.sock"
--   }
--
-- mkMetric :: Int64 -> Int -> MetricRow
-- mkMetric rid workerId = MetricRow
--   { metricRunId       = rid
--   , metricWorkerId    = workerId
--   , metricTimestampMs = 1_700_000_000_000
--   , metricLatencyMs   = 12.5
--   , metricStatusCode  = 200
--   , metricError       = Nothing
--   }


-- spec :: TestTree
-- spec = testGroup "sqlite"
--   [ testCase "init and close db" $ do
--         let tmpDir = "test/tmp_db"
--         createDirectoryIfMissing True tmpDir
--         let dbPath = tmpDir </> "test.db"
--         setEnv "HH200_SQLITE" dbPath
--         conn <- initDb
--         exists <- doesFileExist dbPath
--         assertBool "database file should exist after initDb" exists
--
--         closeDb conn
--         removeDirectoryRecursive tmpDir
--   ]
spec :: TestTree
spec = testGroup "sqlite"
  [ testGroup "connection lifecycle"
      -- (auto)
      [ testCase "init and close db" $ withRawDb $ \_conn dbPath -> do
          exists <- doesFileExist dbPath
          assertBool "database file should exist after initDb" exists
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
-- mustRight :: IO (Either Text a) -> IO a
-- mustRight action = do
--   result <- action
--   case result of
--     Left err -> assertFailure ("expected Right, got Left: " ++ Text.unpack err)
--     Right a  -> pure a

mustJust :: String -> IO (Maybe a) -> IO a
mustJust msg action = do
  result <- action
  case result of
    Nothing -> assertFailure msg
    Just a  -> pure a
