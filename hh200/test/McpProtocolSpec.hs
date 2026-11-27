{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The MCP server at the JSON-RPC level: 'runMcp' on a pair of pipes, fed
-- raw request lines and read back line by line, as an MCP client would.
--
-- The last group drives real runs (a CLI run through 'go', a dashboard run
-- through 'startRun') with the control tools, so pause/resume/stop are
-- checked end to end: MCP -> control socket -> run.
module McpProtocolSpec (spec) where

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Concurrent.MVar
import           Control.Concurrent.STM (atomically, modifyTVar', newTVarIO, readTVarIO)
import           Control.Exception (SomeException, bracket, finally, try)
import           Control.Monad (forM_, unless)
import           Data.Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import           Data.Int (Int64)
import           Data.Maybe (isJust)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Database.SQLite.Simple as Sqlite
import           Network.Wai.Handler.Warp (testWithApplication)
import           System.Directory (doesPathExist, getTemporaryDirectory, removeFile)
import           System.Environment (lookupEnv, setEnv, unsetEnv)
import           System.IO (Handle, hClose, hFlush, openTempFile)
import           System.Process (createPipe)
import           System.Timeout (timeout)
import           Test.Tasty
import           Test.Tasty.HUnit
import qualified Web.Scotty as S

import           DashboardSpec (withScript, withTestDb)
import           Hh200.Cli (Args (duration, source), go, mkArgs)
import           Hh200.McpServer (runMcp, supportedProtocolVersions)
import           Hh200.Runner (RunRequest (..), startRun)

spec :: TestTree
spec = testGroup "McpProtocol"
  [ testGroup "lifecycle"
      [ testInitializeAgrees
      , testInitializeOffersLatest
      , testNotificationsUnanswered
      , testEofEndsServer
      ]
  , testGroup "errors"
      [ testParseError
      , testInvalidUtf8
      , testNotAnObject
      , testUnknownMethod
      , testUnknownTool
      , testUnknownResource
      ]
  , testGroup "tools"
      [ testToolsList
      , testValidateNonAscii
      , testControlWithoutRun
      ]
  , testGroup "control end to end"
      [ testCliStopTwice
      , testCliPauseResume
      , testDashboardRunStop
      , testLiveSocketNotStolen
      ]
  ]

--------------------------------------------------------------------------------
-- Harness
--------------------------------------------------------------------------------

data Client = Client
  { cIn  :: Handle   -- ^ we write requests here
  , cOut :: Handle   -- ^ we read responses here
  , cDone :: MVar ()  -- ^ filled when 'runMcp' returns
  }

-- | Run the server on pipes for the duration of the action.
withMcp :: (Client -> IO a) -> IO a
withMcp act = do
    (srvIn, cliIn) <- createPipe
    (cliOut, srvOut) <- createPipe
    done <- newEmptyMVar
    _ <- forkIO $ (runMcp srvIn srvOut `finally` hClose srvOut) `finally` putMVar done ()
    act (Client cliIn cliOut done) `finally` (try (hClose cliIn) :: IO (Either SomeException ()))

sendRaw :: Client -> BS.ByteString -> IO ()
sendRaw c bs = BS.hPut (cIn c) (bs <> "\n") >> hFlush (cIn c)

send :: Client -> Value -> IO ()
send c = sendRaw c . BL.toStrict . encode

-- | Read one response line, failing the test after 5 s.
recv :: Client -> IO Value
recv c = do
    mLine <- timeout 5000000 (C8.hGetLine (cOut c))
    case mLine of
        Nothing -> assertFailure "no response within 5 s" >> pure Null
        Just line -> case eitherDecodeStrict line of
            Right v -> pure v
            Left e -> assertFailure ("response is not JSON (" ++ e ++ "): " ++ show line) >> pure Null

request :: Int -> Text -> Value -> Value
request i method params = object
    [ "jsonrpc" .= ("2.0" :: Text), "id" .= i, "method" .= method, "params" .= params ]

notification :: Text -> Value
notification method = object [ "jsonrpc" .= ("2.0" :: Text), "method" .= method ]

call :: Client -> Int -> Text -> Value -> IO Value
call c i method params = send c (request i method params) >> recv c

callTool :: Client -> Int -> Text -> Value -> IO Value
callTool c i name args = call c i "tools/call" (object ["name" .= name, "arguments" .= args])

-- | Follow a path of object keys.
at :: [Text] -> Value -> Maybe Value
at [] v = Just v
at (k:ks) (Object o) = KM.lookup (Key.fromText k) o >>= at ks
at _ _ = Nothing

errorCode :: Value -> Maybe Value
errorCode = at ["error", "code"]

-- | The text of a tool result's first content item.
toolText :: Value -> Text
toolText v = case at ["result", "content"] v of
    Just (Array xs) | Just (Object o) <- xs V.!? 0, Just (String t) <- KM.lookup "text" o -> t
    _ -> ""

isToolError :: Value -> Bool
isToolError v = at ["result", "isError"] v == Just (Bool True)

--------------------------------------------------------------------------------
-- Lifecycle
--------------------------------------------------------------------------------

testInitializeAgrees :: TestTree
testInitializeAgrees = testCase "initialize agrees to a supported protocol version" $
    withMcp $ \c -> forM_ supportedProtocolVersions $ \v -> do
        r <- call c 1 "initialize" (object ["protocolVersion" .= v, "capabilities" .= object []])
        assertEqual "id echoed" (Just (Number 1)) (at ["id"] r)
        assertEqual "version" (Just (String v)) (at ["result", "protocolVersion"] r)
        assertEqual "server name" (Just (String "hh200-mcp")) (at ["result", "serverInfo", "name"] r)
        assertBool "advertises tools" (at ["result", "capabilities", "tools"] r /= Nothing)
        assertBool "advertises resources" (at ["result", "capabilities", "resources"] r /= Nothing)

testInitializeOffersLatest :: TestTree
testInitializeOffersLatest = testCase "initialize offers the newest version for an unknown one" $
    withMcp $ \c -> do
        r <- call c 1 "initialize" (object ["protocolVersion" .= ("1999-01-01" :: Text)])
        assertEqual "version" (String <$> take 1 supportedProtocolVersions) (maybe [] pure (at ["result", "protocolVersion"] r))

testNotificationsUnanswered :: TestTree
testNotificationsUnanswered = testCase "notifications get no response, known or not" $
    withMcp $ \c -> do
        send c (notification "notifications/initialized")
        send c (notification "notifications/cancelled")
        send c (notification "no/such/method")
        sendRaw c ""  -- a blank line is skipped too
        r <- call c 7 "ping" (object [])
        assertEqual "the next response answers the ping" (Just (Number 7)) (at ["id"] r)
        assertEqual "ping result" (Just (object [])) (at ["result"] r)

testEofEndsServer :: TestTree
testEofEndsServer = testCase "end of input ends the server" $
    withMcp $ \c -> do
        _ <- call c 1 "ping" (object [])
        hClose (cIn c)
        r <- timeout 5000000 (readMVar (cDone c))
        assertEqual "runMcp returned" (Just ()) r

--------------------------------------------------------------------------------
-- Errors
--------------------------------------------------------------------------------

testParseError :: TestTree
testParseError = testCase "malformed JSON is -32700 with a null id, and the server carries on" $
    withMcp $ \c -> do
        sendRaw c "{not json"
        r <- recv c
        assertEqual "code" (Just (Number (-32700))) (errorCode r)
        assertEqual "id" (Just Null) (at ["id"] r)
        r2 <- call c 2 "ping" (object [])
        assertEqual "still answering" (Just (Number 2)) (at ["id"] r2)

testInvalidUtf8 :: TestTree
testInvalidUtf8 = testCase "invalid UTF-8 is a parse error, not the end of the session" $
    withMcp $ \c -> do
        sendRaw c ("{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ping\",\"x\":\"" <> BS.pack [0xff, 0xfe] <> "\"}")
        r <- recv c
        assertEqual "code" (Just (Number (-32700))) (errorCode r)
        r2 <- call c 2 "ping" (object [])
        assertEqual "still answering" (Just (Number 2)) (at ["id"] r2)

testNotAnObject :: TestTree
testNotAnObject = testCase "a JSON value that isn't a request object is -32600" $
    withMcp $ \c -> do
        sendRaw c "[1,2,3]"
        r <- recv c
        assertEqual "code" (Just (Number (-32600))) (errorCode r)

testUnknownMethod :: TestTree
testUnknownMethod = testCase "an unknown method is -32601" $
    withMcp $ \c -> do
        r <- call c 3 "prompts/list" (object [])
        assertEqual "code" (Just (Number (-32601))) (errorCode r)
        assertEqual "id" (Just (Number 3)) (at ["id"] r)

testUnknownTool :: TestTree
testUnknownTool = testCase "an unknown tool is -32602" $
    withMcp $ \c -> do
        r <- callTool c 4 "launch_rockets" (object [])
        assertEqual "code" (Just (Number (-32602))) (errorCode r)

testUnknownResource :: TestTree
testUnknownResource = testCase "an unknown resource is -32002 with the uri as data" $
    withMcp $ \c -> do
        r <- call c 5 "resources/read" (object ["uri" .= ("hh200://nope" :: Text)])
        assertEqual "code" (Just (Number (-32002))) (errorCode r)
        assertEqual "data.uri" (Just (String "hh200://nope")) (at ["error", "data", "uri"] r)

--------------------------------------------------------------------------------
-- Tools
--------------------------------------------------------------------------------

testToolsList :: TestTree
testToolsList = testCase "tools/list names the four tools" $
    withMcp $ \c -> do
        r <- call c 1 "tools/list" (object [])
        let names = case at ["result", "tools"] r of
                Just (Array xs) -> [ n | Object o <- V.toList xs, Just (String n) <- [KM.lookup "name" o] ]
                _ -> []
        assertEqual "names" ["pause", "resume", "stop", "validate_script"] names

-- | The request arrives as UTF-8 bytes whatever the locale; the diagnostic
-- quotes the offending token, so a mangled decode would show here.
testValidateNonAscii :: TestTree
testValidateNonAscii = testCase "validate_script sees non-ASCII text intact" $
    withMcp $ \c -> do
        ok <- callTool c 1 "validate_script"
            (object ["script" .= ("POST http://localhost:9999/echo\n{ \"s\": \"日本語 é ✓\" }\nHTTP 200\n" :: Text)])
        assertBool "not a tool error" (not (isToolError ok))
        assertEqual "valid" (Just (Bool True)) (decodeText (toolText ok) >>= at ["ok"])
  where
    decodeText = decodeStrict . TE.encodeUtf8

testControlWithoutRun :: TestTree
testControlWithoutRun = testCase "a control tool with no run says so" $
    withSocketPath $ \_ -> withMcp $ \c -> do
        r <- callTool c 1 "pause" (object [])
        assertBool "tool error" (isToolError r)
        assertBool ("explains: " ++ T.unpack (toolText r)) ("no hh200 run is listening" `T.isInfixOf` toolText r)

--------------------------------------------------------------------------------
-- Control, end to end
--------------------------------------------------------------------------------

-- | Point @HH200_CONTROL_SOCKET@ at a fresh, unused path.
withSocketPath :: (FilePath -> IO a) -> IO a
withSocketPath act = do
    tmp <- getTemporaryDirectory
    (fp, h) <- openTempFile tmp "hh200-mcp-uds"
    hClose h
    removeFile fp
    setEnv "HH200_CONTROL_SOCKET" fp
    act fp `finally` do
        unsetEnv "HH200_CONTROL_SOCKET"
        _ <- try (removeFile fp) :: IO (Either SomeException ())
        pure ()

-- | A server that counts the requests it gets, on an ephemeral port.
withCountingServer :: (IO Int -> Int -> IO a) -> IO a
withCountingServer act = do
    hits <- newTVarIO (0 :: Int)
    app <- S.scottyApp $ S.get "/ok" $ do
        S.liftIO (atomically (modifyTVar' hits (+ 1)))
        S.text "ok"
    testWithApplication (pure app) (act (readTVarIO hits))

loopScript :: Int -> String
loopScript port = "GET http://127.0.0.1:" ++ show port ++ "/ok\nHTTP 200\n"

-- | Wait until @p@ holds, checking every 50 ms, for up to @secs@ seconds.
eventually :: Int -> IO Bool -> IO Bool
eventually secs p = poll (secs * 20)
  where
    poll 0 = p
    poll n = do
        ok <- p
        if ok then pure True else threadDelay 50000 >> poll (n - 1 :: Int)

-- | Start a 30 s CLI load run in a thread; return a var filled when 'go' ends.
startCliRun :: FilePath -> IO (MVar (Either SomeException ()))
startCliRun script = do
    finished <- newEmptyMVar
    _ <- forkIO $ try (go mkArgs { source = Just script, duration = 30 }) >>= putMVar finished
    pure finished

-- | Wait for the run's listener to appear.
awaitSocket :: FilePath -> IO ()
awaitSocket sock = do
    up <- eventually 5 (doesPathExist sock)
    unless up $ assertFailure ("run never bound " ++ sock)

-- | Two runs in a row on one socket path: the second must still be
-- controllable, and each run removes its socket file when it ends.
testCliStopTwice :: TestTree
testCliStopTwice = testCase "stop ends a CLI run, twice in a row on the same socket" $
    withSocketPath $ \sock -> withTestDb False $ withCountingServer $ \hits port ->
        withScript (loopScript port) $ \script -> withMcp $ \c ->
            forM_ [1, 2 :: Int] $ \n -> do
                finished <- startCliRun script
                awaitSocket sock
                sending <- eventually 5 ((> 0) <$> hits)
                assertBool "the run is sending requests" sending
                r <- callTool c n "stop" (object [])
                assertBool ("stop accepted (run " ++ show n ++ "): " ++ T.unpack (toolText r)) (not (isToolError r))
                ended <- timeout 5000000 (takeMVar finished)
                case ended of
                    Nothing -> assertFailure ("run " ++ show n ++ " ignored stop")
                    Just (Left e) -> assertFailure ("run " ++ show n ++ " failed: " ++ show e)
                    Just (Right ()) -> pure ()
                gone <- eventually 2 (not <$> doesPathExist sock)
                assertBool "the run removes its socket file" gone

testCliPauseResume :: TestTree
testCliPauseResume = testCase "pause holds a CLI run and resume restarts it" $
    withSocketPath $ \sock -> withTestDb False $ withCountingServer $ \hits port ->
        withScript (loopScript port) $ \script -> withMcp $ \c -> do
            finished <- startCliRun script
            awaitSocket sock
            _ <- eventually 5 ((> 0) <$> hits)
            r <- callTool c 1 "pause" (object [])
            assertBool "pause accepted" (not (isToolError r))
            threadDelay 300000  -- let in-flight requests land
            before <- hits
            threadDelay 500000
            during <- hits
            assertEqual "no requests while paused" before during
            _ <- callTool c 2 "resume" (object [])
            moving <- eventually 5 ((> during) <$> hits)
            assertBool "requests again after resume" moving
            _ <- callTool c 3 "stop" (object [])
            ended <- timeout 5000000 (takeMVar finished)
            assertBool "run ended after stop" (isJust ended)

-- | Dashboard runs listen on the control socket too.
testDashboardRunStop :: TestTree
testDashboardRunStop = testCase "stop ends a dashboard run" $
    withSocketPath $ \sock -> withTestDb False $ withCountingServer $ \hits port ->
        withScript (loopScript port) $ \script -> withMcp $ \c -> do
            started <- startRun (RunRequest "mcp stop" (T.pack script) 1 30 0)
            rid <- either (\e -> assertFailure ("startRun: " ++ show e) >> pure 0) pure started
            awaitSocket sock
            _ <- eventually 5 ((> 0) <$> hits)
            r <- callTool c 1 "stop" (object [])
            assertBool ("stop accepted: " ++ T.unpack (toolText r)) (not (isToolError r))
            done <- eventually 5 ((== Just "completed") <$> runStatus rid)
            assertBool "run marked completed well before its 30 s" done
            gone <- eventually 2 (not <$> doesPathExist sock)
            assertBool "the run removes its socket file" gone

-- | A second run must not unlink the socket of a run that is still alive.
testLiveSocketNotStolen :: TestTree
testLiveSocketNotStolen = testCase "a second run leaves a live run's socket alone" $
    withSocketPath $ \sock -> withTestDb False $ withCountingServer $ \hits port ->
        withScript (loopScript port) $ \script -> withMcp $ \c -> do
            first <- startCliRun script
            awaitSocket sock
            _ <- eventually 5 ((> 0) <$> hits)
            -- The second run warns and carries on without a listener; it is
            -- short so it ends on its own.
            started <- startRun (RunRequest "second" (T.pack script) 1 2 0)
            rid <- either (\e -> assertFailure ("startRun: " ++ show e) >> pure 0) pure started
            threadDelay 200000
            _ <- callTool c 1 "stop" (object [])
            ended <- timeout 5000000 (takeMVar first)
            assertBool "the first run still got stop" (isJust ended)
            status <- runStatus rid
            assertEqual "the second run is unaffected" (Just "running") status
            settled <- eventually 5 ((== Just "completed") <$> runStatus rid)
            assertBool "the second run ends on its own timer" settled

-- | A run's status column, read straight from the test database. The run's
-- writer thread may hold the lock, so wait on it and treat a still-busy
-- database as "not known yet" ('eventually' retries).
runStatus :: Int64 -> IO (Maybe Text)
runStatus rid = do
    r <- try $ bracket openDb Sqlite.close $ \conn -> do
        Sqlite.execute_ conn "PRAGMA busy_timeout = 2000"
        rows <- Sqlite.query conn "SELECT status FROM runs WHERE id = ?" (Sqlite.Only rid)
        pure $ case rows of
            [Sqlite.Only s] -> Just s
            _ -> Nothing
    pure $ either (\(_ :: Sqlite.SQLError) -> Nothing) id r
  where
    openDb = do
        fp <- maybe (fail "HH200_SQLITE unset") pure =<< lookupEnv "HH200_SQLITE"
        Sqlite.open fp
