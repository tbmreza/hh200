{-# LANGUAGE OverloadedStrings #-}

module DashboardSpec (spec) where

import           Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, takeMVar)
import           Control.Exception (finally)
import           Control.Monad (when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.CaseInsensitive as CI
import qualified Database.SQLite.Simple as Sqlite
import qualified Network.HTTP.Client as HC
import           Network.HTTP.Types.Status (statusCode)
import           Network.Socket
import qualified Network.Socket.ByteString as NBS
import           Network.Wai.Handler.Warp (testWithApplication)
import           System.Directory (getTemporaryDirectory, removeFile)
import           System.Environment (setEnv, unsetEnv)
import           System.IO (hClose, openTempFile)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Web.Scotty (scottyApp)

import           Hh200.Dashboard (dashboardRoutes)

--------------------------------------------------------------------------------
-- Harness
--------------------------------------------------------------------------------

-- | Run the dashboard WAI app on an ephemeral port and hand the caller a
-- manager + port. The server is torn down when the callback returns.
withDashboard :: Bool -> (HC.Manager -> Int -> IO a) -> IO a
withDashboard isDev act = do
    mgr <- HC.newManager HC.defaultManagerSettings
    testWithApplication (scottyApp (dashboardRoutes isDev)) (act mgr)
        `finally` HC.closeManager mgr

getLbs :: HC.Manager -> Int -> String -> IO (HC.Response BL.ByteString)
getLbs mgr port path = do
    req <- HC.parseRequest ("http://127.0.0.1:" ++ show port ++ path)
    HC.httpLbs req mgr

postLbs :: HC.Manager -> Int -> String -> BL.ByteString -> IO (HC.Response BL.ByteString)
postLbs mgr port path body = do
    req <- HC.parseRequest ("http://127.0.0.1:" ++ show port ++ path)
    let req' = req { HC.method = "POST", HC.requestBody = HC.RequestBodyLBS body }
    HC.httpLbs req' mgr

assertStatus :: Int -> HC.Response a -> IO ()
assertStatus expected resp =
    assertEqual ("expected status " ++ show expected) expected (statusCode (HC.responseStatus resp))

assertBodyContains :: String -> HC.Response BL.ByteString -> IO ()
assertBodyContains needle resp =
    assertBool ("body should contain " ++ show needle)
        (BS.isInfixOf (BSC.pack needle) (BL.toStrict (HC.responseBody resp)))

headerValue :: String -> HC.Response a -> Maybe BS.ByteString
headerValue name resp = lookup (CI.mk (BSC.pack name)) (HC.responseHeaders resp)

--------------------------------------------------------------------------------
-- Database fixtures
--------------------------------------------------------------------------------

schema :: [Sqlite.Query]
schema =
    [ "CREATE TABLE IF NOT EXISTS runs ( \
      \id INTEGER PRIMARY KEY AUTOINCREMENT, \
      \name TEXT, script_path TEXT, started_at INTEGER, ended_at INTEGER, \
      \status TEXT, concurrency INTEGER, rate_limit REAL, control_socket TEXT)"

    , "CREATE TABLE IF NOT EXISTS metric_windows ( \
      \id INTEGER PRIMARY KEY AUTOINCREMENT, run_id INTEGER)"
    ]

-- | Point @HH200_SQLITE@ at a fresh temp file, create the schema, optionally
-- seed a run + metric window, run the action, then clean up.
withTestDb :: Bool -> IO a -> IO a
withTestDb seeded act = do
    tmp <- getTemporaryDirectory
    (fp, h) <- openTempFile tmp "hh200-db"
    hClose h
    setEnv "HH200_SQLITE" fp
    conn <- Sqlite.open fp
    mapM_ (Sqlite.execute_ conn) schema
    when seeded $ do
        Sqlite.execute conn
            "INSERT INTO runs (name, script_path, started_at, ended_at, status, concurrency, rate_limit, control_socket) \
            \VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
            ( "my run" :: String
            , "/tmp/x.hhs" :: String
            , 1234567890 :: Int
            , Nothing :: Maybe Int
            , "running" :: String
            , 5 :: Int
            , 10.5 :: Double
            , "/tmp/cs" :: String
            )
        Sqlite.execute conn
            "INSERT INTO metric_windows (run_id) VALUES (?)"
            (Sqlite.Only (1 :: Int))
    Sqlite.close conn
    act `finally` do
        unsetEnv "HH200_SQLITE"
        removeFile fp

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

spec :: TestTree
spec = testGroup "Dashboard"
  [ testRunsEmpty
  , testRunsSeeded
  , testReportSeeded
  , testReportEmpty
  , testSigForwards
  , testSse
  , testStaticProd
  , testStaticDev
  ]

testRunsEmpty :: TestTree
testRunsEmpty = testCase "GET /api/runs returns an empty list" $ do
    withTestDb False $ do
        withDashboard False $ \mgr port -> do
            resp <- getLbs mgr port "/api/runs"
            assertStatus 200 resp
            assertBodyContains "\"runs\":[]" resp

testRunsSeeded :: TestTree
testRunsSeeded = testCase "GET /api/runs returns seeded rows" $ do
    withTestDb True $ do
        withDashboard False $ \mgr port -> do
            resp <- getLbs mgr port "/api/runs"
            assertStatus 200 resp
            assertBodyContains "\"name\":\"my run\"" resp
            assertBodyContains "\"started_at\":1234567890" resp
            assertBodyContains "\"ended_at\":null" resp
            assertBodyContains "\"status\":\"running\"" resp
            assertBodyContains "\"concurrency\":5" resp
            assertBodyContains "\"rate_limit\":10.5" resp

testReportSeeded :: TestTree
testReportSeeded = testCase "GET /api/report/:runId returns a CSV download" $ do
    withTestDb True $ do
        withDashboard False $ \mgr port -> do
            resp <- getLbs mgr port "/api/report/1"
            assertStatus 200 resp
            assertEqual "csv content-type" (Just "text/csv") (headerValue "Content-Type" resp)
            let disp = headerValue "Content-Disposition" resp
            assertBool "attachment disposition" $
                maybe False (BS.isInfixOf "attachment; filename=\"stats_history_1.csv\"") disp
            assertBodyContains "run_name,metric_window_rps" resp
            assertBodyContains "my run,1" resp

testReportEmpty :: TestTree
testReportEmpty = testCase "GET /api/report/:runId with no metrics returns header only" $ do
    withTestDb True $ do
        withDashboard False $ \mgr port -> do
            resp <- getLbs mgr port "/api/report/999"
            assertStatus 200 resp
            assertBodyContains "run_name,metric_window_rps" resp
            assertBool "no metric rows for unknown id" $
                not (BS.isInfixOf "my run" (BL.toStrict (HC.responseBody resp)))

testSigForwards :: TestTree
testSigForwards = testCase "POST /api/sig forwards the body to the control socket" $ do
    tmp <- getTemporaryDirectory
    (fp, h) <- openTempFile tmp "hh200-uds"
    hClose h
    removeFile fp
    setEnv "HH200_CONTROL_SOCKET" fp
    (withUdsListener fp $ \received ->
        withDashboard False $ \mgr port -> do
            resp <- postLbs mgr port "/api/sig" (BL.fromStrict "pause")
            assertStatus 200 resp
            assertBodyContains "\"ok\"" resp
            msg <- takeMVar received
            assertEqual "forwarded bytes" "pause" msg)
        `finally` unsetEnv "HH200_CONTROL_SOCKET"

-- | Bind a Unix socket, accept one connection, read until EOF, and store the
-- received bytes in the returned MVar.
withUdsListener :: FilePath -> (MVar BS.ByteString -> IO a) -> IO a
withUdsListener path act = do
    mvar <- newEmptyMVar
    s <- socket AF_UNIX Stream defaultProtocol
    bind s (SockAddrUnix path)
    listen s 1
    _ <- forkIO $ do
        (conn, _) <- accept s
        bs <- recvAll conn
        putMVar mvar bs
        close conn
        close s
    act mvar `finally` close s
  where
    recvAll conn = go BS.empty
      where
        go acc = do
            chunk <- NBS.recv conn 4096
            if BS.null chunk then pure acc else go (acc <> chunk)

testSse :: TestTree
testSse = testCase "GET /api/sse streams event-source frames" $ do
    withDashboard False $ \mgr port -> do
        req <- HC.parseRequest ("http://127.0.0.1:" ++ show port ++ "/api/sse")
        HC.withResponse req mgr $ \resp -> do
            assertStatus 200 resp
            assertEqual "event-stream content-type"
                (Just "text/event-stream") (headerValue "Content-Type" resp)
            assertEqual "no-cache cache-control"
                (Just "no-cache") (headerValue "Cache-Control" resp)
            chunk <- readUntilFrame (HC.responseBody resp)
            assertBool "frame begins with data:" ("data: " `BS.isPrefixOf` chunk)
            assertBool "frame carries a JSON payload" ("nested" `BS.isInfixOf` chunk)

-- | Read from the SSE body reader until a full @\\n\\n@-terminated frame is
-- collected (or the stream ends / a cap is hit). The endpoint never closes, so
-- we must not block forever.
readUntilFrame :: HC.BodyReader -> IO BS.ByteString
readUntilFrame br = go BS.empty (10 :: Int)
  where
    go acc 0 = pure acc
    go acc n = do
        chunk <- HC.brRead br
        if BS.null chunk
            then pure acc
            else if "\n\n" `BS.isInfixOf` (acc <> chunk)
                then pure (acc <> chunk)
                else go (acc <> chunk) (n - 1)

testStaticProd :: TestTree
testStaticProd = testCase "prod serves the SvelteKit SPA and static assets" $ do
    withDashboard False $ \mgr port -> do
        rRoot <- getLbs mgr port "/"
        assertStatus 200 rRoot
        assertBodyContains "<!doctype html>" rRoot
        assertBodyContains "__sveltekit" rRoot

        rRoute <- getLbs mgr port "/some/client/route"
        assertStatus 200 rRoute
        assertBodyContains "<!doctype html>" rRoute

        rRobots <- getLbs mgr port "/robots.txt"
        assertStatus 200 rRobots
        assertBodyContains "User-agent" rRobots

testStaticDev :: TestTree
testStaticDev = testCase "dev does not serve the SPA file" $ do
    withDashboard True $ \mgr port -> do
        resp <- getLbs mgr port "/"
        assertStatus 200 resp
        assertEqual "empty body in dev" "" (L8.unpack (HC.responseBody resp))
