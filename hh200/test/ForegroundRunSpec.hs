{-# LANGUAGE OverloadedStrings #-}

-- | A CLI run ('runForeground', what @hh200 script.hhs@ does) is recorded like
-- a dashboard run: its @runs@ row gets a final status and @ended_at@, and its
-- requests are captured. Read back from SQLite.
module ForegroundRunSpec (spec) where

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Exception (finally)
import           Control.Monad (forM_, void)
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.HashMap.Strict as HM
import           Data.List (sort)
import qualified Data.Text as T
import           GHC.Clock (getMonotonicTime)
import           Network.Wai.Handler.Warp (testWithApplication)
import           System.Directory (getTemporaryDirectory, removeFile)
import           System.Environment (setEnv, unsetEnv)
import           System.IO (hClose, openTempFile)
import           Test.Tasty
import           Test.Tasty.HUnit
import qualified Web.Scotty as S

import           DashboardSpec (withTestDb)
import           Hh200.Database
import           Hh200.Inputs (FilePolicy (..), RunInputs (..))
import           Hh200.Runner (ForegroundRun (..), runForeground)
import           Hh200.Scanner (analyze)
import           Hh200.Types (Snippet (..))

spec :: TestTree
spec = testGroup "CLI runs are recorded"
    [ testCase "a passing run is completed, with ended_at and its request captured" $
        withRun $ \port -> do
            (failed, run, reqs) <- foreground port Nothing 1 "GET http://127.0.0.1:PORT/ok\nHTTP 200\n"
            assertBool "no VU failed" (not failed)
            assertEqual "status" "completed" (runStatus run)
            assertEqual "name" "cli.hhs" (runName run)
            assertEqual "script path" "/scripts/cli.hhs" (runScriptPath run)
            assertEnded run
            r <- case reqs of
                [one] -> pure one
                _ -> assertFailure ("expected one request, got " ++ show (length reqs)) >> fail "requests"
            let row = detailRow r
            assertEqual "method" "GET" (requestMethod row)
            assertEqual "url" (T.pack ("http://127.0.0.1:" ++ show port ++ "/ok")) (requestUrl row)
            assertEqual "status code" (Just 200) (requestStatus row)
            assertEqual "response body captured" [Just "fine"]
                [ requestBodyFullContent b | b <- detailBodies r, requestBodyFullDirection b == "response" ]

    , testCase "a failing assertion marks the run failed and still captures the request" $
        withRun $ \port -> do
            (failed, run, reqs) <- foreground port Nothing 1 "GET http://127.0.0.1:PORT/ok\nHTTP 201\n"
            assertBool "a VU failed" failed
            assertEqual "status" "failed" (runStatus run)
            assertEnded run
            assertEqual "the request is captured" [Just 200] (map (requestStatus . detailRow) reqs)

    , testCase "each virtual user's request is captured under its worker id" $
        withRun $ \port -> do
            (_, run, reqs) <- foreground port Nothing 2 "GET http://127.0.0.1:PORT/ok\nHTTP 200\n"
            assertEqual "status" "completed" (runStatus run)
            assertEqual "concurrency" 2 (runConcurrency run)
            assertEqual "worker ids" [Just 0, Just 1] (sort (map (requestWorkerId . detailRow) reqs))

    , testCase "stopping with a request still in flight is cancelled, without waiting for it" $
        withRun $ \port -> do
            t0 <- getMonotonicTime
            (failed, run, _) <- foreground port (Just 300000) 1 "GET http://127.0.0.1:PORT/slow\nHTTP 200\n"
            t1 <- getMonotonicTime
            assertBool "no VU failed" (not failed)
            assertEqual "status" "cancelled" (runStatus run)
            assertEnded run
            assertBool ("returned within the grace period, took " ++ show (t1 - t0) ++ " s")
                (t1 - t0 < 6)
    ]

-- | Parse the script (PORT replaced), run it in the foreground, then read the
-- (only) run and its requests back from the database. @stopAfter@ stops the
-- run that many microseconds after it starts; 'Nothing' lets it end on its own.
foreground :: Int -> Maybe Int -> Int -> String -> IO (Bool, RunRow, [RequestDetail])
foreground port stopAfter nvu src = do
    parsed <- analyze (Snippet (L8.pack (replacePort src)))
    script <- either (\e -> assertFailure ("fixture does not parse: " ++ e) >> fail "unparsed") pure parsed
    let onStart stop = forM_ stopAfter $ \micros ->
            void $ forkIO (threadDelay micros >> stop)
    failed <- runForeground onStart ForegroundRun
        { frName = "cli.hhs"
        , frScriptPath = "/scripts/cli.hhs"
        , frInputs = RunInputs { riVars = HM.empty, riFiles = FilePolicy { fpBaseDir = ".", fpRoot = Nothing } }
        , frScript = script
        , frNvu = nvu
        , frDuration = 0
        }
    conn <- initDb
    runs <- listRuns conn
    run <- case runs of
        [r] -> pure r
        _ -> assertFailure ("expected one run, got " ++ show (length runs)) >> fail "runs"
    reqs <- either (\e -> assertFailure (T.unpack e) >> fail "requests") pure =<< listRequestsByRun conn (runId run)
    closeDb conn
    pure (failed, run, reqs)
  where
    replacePort s = T.unpack (T.replace "PORT" (T.pack (show port)) (T.pack s))

assertEnded :: RunRow -> Assertion
assertEnded run = case runEndedAt run of
    ETStillRunning -> assertFailure "ended_at is still NULL"
    ETHasEnded t -> assertBool "ended_at >= started_at" (t >= runStartedAt run)

-- | A fresh database, a private control socket, and the fixture server.
withRun :: (Int -> IO a) -> IO a
withRun act = withTestDb False $ withSocket $ testWithApplication (S.scottyApp app) act
  where
    app = do
        S.get "/ok" $ S.text "fine"
        S.get "/slow" $ do
            S.liftIO (threadDelay 10000000)
            S.text "late"

withSocket :: IO a -> IO a
withSocket act = do
    tmp <- getTemporaryDirectory
    (fp, h) <- openTempFile tmp "hh200-fg-uds"
    hClose h
    removeFile fp
    setEnv "HH200_CONTROL_SOCKET" fp
    act `finally` unsetEnv "HH200_CONTROL_SOCKET"
