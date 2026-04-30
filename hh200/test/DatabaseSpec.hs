{-# LANGUAGE OverloadedStrings #-}

module DatabaseSpec (spec) where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Database.SQLite.Simple
import           Control.Concurrent.STM
import           Control.Concurrent (threadDelay)
import qualified Data.Text as T
import qualified Hh200.Database as DB

spec :: TestTree
spec = testGroup "Database Tests"
    [ testCase "Initialize database and insert data" $ do
        conn <- open ":memory:"
        DB.initDatabase conn

        q <- newTQueueIO
        DB.startDbWriter conn q

        let runId = "test-run-1"
        atomically $ writeTQueue q $ Just $ DB.AddRun $ DB.Run runId 123456789 "{}"

        atomically $ writeTQueue q $ Just $ DB.AddRequest $ DB.Request
            { DB.reqRunId = runId
            , DB.requestId = "req-1"
            , DB.workerId = 1
            , DB.srcPort = 12345
            , DB.url = "http://example.com"
            , DB.method = "GET"
            , DB.status = 200
            , DB.bodyBytes = 100
            , DB.tStart = 1000
            , DB.tEnd = 2000
            , DB.durationMs = 1.0
            }

        atomically $ writeTQueue q $ Just $ DB.AddTcpEvent $ DB.TcpEvent
            { DB.tcpRunId = runId
            , DB.tcpSrcPort = 12345
            , DB.tcpEventType = "retransmit"
            , DB.tcpValue = 1.0
            , DB.kernelTs = 5000
            }

        -- Signal termination and wait for it
        atomically $ writeTQueue q Nothing

        -- Instead of flaky threadDelay, we can poll for the termination signal
        -- but since our current startDbWriter doesn't have an easy way to signal back
        -- (like an MVar), we'll do a slightly more robust check or just wait.
        -- Given the constraints, let's keep a small delay and then verify.
        threadDelay 50000

        [OnlyCount countRuns] <- query_ conn "SELECT count(*) FROM runs"
        [OnlyCount countReqs] <- query_ conn "SELECT count(*) FROM requests"
        [OnlyCount countTcp]  <- query_ conn "SELECT count(*) FROM tcp_events"

        assertEqual "Should have 1 run" (1 :: Int) countRuns
        assertEqual "Should have 1 request" (1 :: Int) countReqs
        assertEqual "Should have 1 tcp event" (1 :: Int) countTcp

        close conn
    ]

newtype OnlyCount = OnlyCount Int
instance FromRow OnlyCount where
    fromRow = OnlyCount <$> field
