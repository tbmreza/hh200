{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Hh200.Cli
  ( cli
  , go, Args(..), optsInfo
  , mkArgs
  ) where

import Debug.Trace

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Maybe
import           Control.Exception (finally, try, SomeException)
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TQueue (flushTQueue)
import           Control.Monad (forM_, replicateM, when, forever)
import           System.Posix.Signals (installHandler, sigINT, Handler(CatchOnce))
import           System.Exit (exitWith, ExitCode(ExitFailure))
import           System.IO (hPutStrLn, stderr, stdout)
import qualified System.IO (hFlush)
import           System.Directory (doesFileExist)
import           Web.Scotty (scotty, regex)
import qualified Web.Scotty as Server
import           Network.Wai.Middleware.Static (staticPolicy, addBase)
import           Options.Applicative
import           Data.Aeson (ToJSON (..), object, (.=))
import           Data.Int (Int64)
import           Data.Text (Text, pack)
import           Data.Version (showVersion)
import           Database.SQLite.Simple (FromRow (..), field, query_, Connection, execute, lastInsertRowId)
import qualified Paths_hh200 (version)

import Network.Socket
import qualified Network.Socket.ByteString as NBS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import Control.Monad (unless)
import System.Directory (removeFile, doesFileExist)
import Control.Exception  (bracket)
import Network.Socket     (Family (..), SockAddr (..), SocketType (..),
                           accept, bind, close, defaultProtocol, listen,
                           socket, socketToHandle)
import System.Directory   (removePathForcibly)
import System.IO          (BufferMode (..), Handle, IOMode (..), hClose,
                           hSetBuffering)

import qualified Hh200.TokenBucketWorkerPool as Tbwp (wcWorkerId, wcRateLimiter, wcMode, WorkerConfig(..), worker, withRateLimiter, dummyDuo, WorkerMode(..))
import           Hh200.TokenBucketWorkerPool (RunState(..), worker, courier)
import           Hh200.Types
import qualified Hh200.Scanner as Scanner
import           Hh200.Database (initDb, closeDb)
import           Hh200.LanguageServer (runTcp, runStdio)


data Args = Args
  { source :: Maybe String  -- used for both FilePath and Snippet sources
  , version :: Bool
  , debugConfig :: Bool
  , call :: Bool
  , nvu :: Int
  , duration :: Int
  , lsp :: Maybe Int
  , lspStdio :: Bool
  , browse :: Maybe Int -- browse mode port (Nothing = not browse)
  } deriving (Show, Eq)

cli :: IO ()
cli = go =<< execParser optsInfo

optsInfo :: ParserInfo Args
optsInfo = info (helper <*>   modeBrowse <|> modeA)
                (fullDesc <>  header "Run hh200 scripts")
    where
    modeBrowse :: Parser Args
    modeBrowse = subparser $
        command "browse" $
            info (((\p -> mkArgs { browse = Just p }) <$>
                   option auto ( long "port"
                              <> short 'p'
                              <> help "HTTP port for dashboard"
                              <> value 8089
                              <> showDefault ))
                  <**> helper)
                 (progDesc "Launch the dashboard")

    applyMods :: Args -> Maybe (Args -> Args) -> Args
    applyMods args (Just f) = f args
    applyMods args Nothing  = args

    modeA :: Parser Args
    modeA = applyMods
        <$> (Args
        <$> optional (argument str (metavar "SOURCE"
                                 <> help "Path of source program"))

        <*> switch ( long "version"
                  <> short 'V'
                  <> help "Print version info and exit" )

        <*> switch ( long "debug-config"
                  <> short 'F'
                  <> help "Read environment and script header to determine the config values without executing script's side-effects" )

        <*> switch ( long "call"
                  <> short 'C'
                  <> help "Execute a script snippet directly" )

        <*> option auto ( long "nvu"
                       <> short 'n'
                       <> help "Number of virtual users"
                       <> metavar "N"
                       <> value 1
                       <> showDefault )

        <*> option auto ( long "duration"
                       <> short 't'
                       <> help "Set duration of load test execution in seconds"
                       <> metavar "S"
                       <> value 0
                       <> showDefault )

        <*> optional ( option auto ( long "lsp"
                                   <> short 'd'
                                   <> help "Run hh200 language server"
                                   <> metavar "PORT" ) )

        <*> switch ( long "lsp-stdio"
                  <> help "Run hh200 language server over stdio" )

        -- (auto)
        <*> pure Nothing)
        <*> optional ((\n a -> a { nvu = n, duration = 0 }) <$>
                      option auto ( long "shotgun"
                                 <> help "Alias for --nvu=N --duration=0"
                                 <> metavar "N" ))

-- go and goStraight indirection: debug programming directly in Script structs.
go :: Args -> IO ()

-- Print executable version.
-- hh200 --version
go Args { version = True } = do
    putStrLn $ showVersion Paths_hh200.version
    System.IO.hFlush stdout

-- Run language server.
-- hh200 --lsp=3000
go Args { lsp = Just port } = runTcp port

-- Run language server over stdio.
-- hh200 --lsp-stdio
go Args { lspStdio = True } = runStdio

-- Browse/dashboard mode.
-- hh200 browse
-- hh200 browse --port 9090
go Args { browse = Just port } = startServer (show port)

-- Static-check script.
-- hh200 flow.hhs --debug-config
go Args { source = Just path, debugConfig = True } = do
    let analyzed = Scanner.analyze path
    m <- runMaybeT analyzed
    case m of
        _ -> undefined

-- Script execution.
-- hh200 flow.hhs
-- go args@Args { duration = 1, call = False, rps = False, source = Just path } = do
go args@Args { duration = 1, call = False, source = Just path } = do
    exists <- doesFileExist path
    if not exists then do
        hPutStrLn stderr $ "error: file not found: " ++ path
        exitWith (ExitFailure 1)
    else do
        let analyzed = Scanner.analyze path
        m <- runMaybeT analyzed
        case m of
            -- Just script -> trace ("path=" ++ show path) $ goStraight script args
            Just script -> trace ("goMode path=" ++ show path) $ goMode script args
            _ -> error "bug in hh200 grammar!"

-- Inline program execution.
-- hh200 --call "GET ..."
go Args { call = True, source = Just snip } =
    error ("undefined --call snip=" ++ snip)

-- -- Inserts timeseries data to a file database and optionally serves a web frontend.
-- -- hh200 flow.hhs --rps
-- go Args { rps = True, duration = n, source = Just path } = do
--     mScript <- runMaybeT (Scanner.analyze path)
--     case mScript of
--         Nothing -> exitWith (ExitFailure 1)
--         -- Just s  -> testRps rpsVal concurrency rampUpUs thinkTimeUs script
--         Just s  -> testRps 10 n 1000000 500000 s

-- Shotgun.
-- hh200 flow.hhs --nvu=4 --duration=0
        -- Just script  -> testShotgun n script
-- Load test mode.
-- hh200 flow.hhs --duration=30
go args@Args { duration = n, call = False, source = Just path } = do
    mScript <- runMaybeT (Scanner.analyze path)
    case mScript of
        Nothing -> exitWith (ExitFailure 1)
        Just script  -> trace "this goMode!!!!" $ goMode script args

-- Verifiable with `echo $?` which prints last exit code in shell.
go _ = exitWith (ExitFailure 1)

genName = "default run name"

-- staggerDelayMicr :: Int -> Int -> Int
-- staggerDelayMicr n i
--     | n >= 4    = i * (1_000_000 `div` n)   -- spread evenly across 1 s
--     | otherwise = 0

-- listenUds :: FilePath -> (Handle -> IO ()) -> IO ()
-- listenUds path handler = do
--     removePathForcibly path
--     bracket (socket AF_UNIX Stream defaultProtocol) close $ \srv -> do
--         bind srv (SockAddrUnix path)
--         listen srv 1
--         (client, _addr) <- accept srv
--         bracket (socketToHandle client ReadMode) hClose $ \h -> do
--             hSetBuffering h LineBuffering
--             handler h


goMode :: Script -> Args -> IO ()

goMode script args = do
    testSimple

    where
    -- terminate :: Flag -> IO ()
    terminate v = atomically $ writeTVar v Stopped

    mkRunRow :: IO RunRow
    mkRunRow = do
        pure $ RunRow
          { runControlSocket = ""
          , runStatus = "running"
          , runEndedAt = ETStillRunning
          -- args fields --
          , runRateLimit = 0.0
          , runScriptPath = ""
          -- system io --
          , runName = genName
          , runStartedAt = 0
          }

    -- Script of single call item, repeatedly fired for duration unless
    -- interrupted.
    testSimple :: IO ()
    testSimple = do
        conn <- initDb

        rr <- mkRunRow

        mRunId <- insertRun conn rr

        -- Control flag.
        s <- newTVarIO Running

        let udsFile = "/tmp/uds_socket"

        -- The loop that fires HTTP requests.
        -- ??: Automatic 1-second stagger for nvu >= 4. Starting point could be
        -- to assume sequencing N imperative forkIO $ courier ... statements
        -- starts N couriers at the same instant.
        let argsNvu = nvu args
        drawer <- replicateM argsNvu newEmptyMVar
        -- forM_ (zip undefined drawer) $ \(_, hole) -> do
        forM_ (drawer) $ \(hole) -> do
            forkIO $ courier script (s, duration args) hole

        -- Termination with ctrl+c, which is handled foremostly by worker.
        _ <- installHandler sigINT
                            (CatchOnce $ terminate s)
                            Nothing

        -- Unix Domain Socket listener
        _ <- forkIO $
            controlSocketListener "/tmp/uds_socket" $ \msg ->  -- ??: xdg comply
                case msg of
                    "pause\n" -> undefined
                    "resume\n" -> atomically $ writeTVar s Running
                    -- "stop\n" -> terminate s
                    "hello from writer\n" -> terminate s
                    "hello from writer" -> terminate s
                    _        -> putStrLn $ "received: " ++ msg

        -- Termination when all workers are done.
        _ <- forkIO $ do
            forM_ drawer readMVar
            terminate s

        -- Termination based on timer only when duration > 0.
        when (duration args > 0) $ do
            _ <- forkIO $ do
                threadDelay ((duration args) * 1000000)
                terminate s
            pure ()

        atomically $
            check . (== Stopped) =<< readTVar s


controlSocketListener
    :: FilePath
    -> (String -> IO ())
    -> IO ()
controlSocketListener path handler = do
    sock <- socket AF_UNIX Stream defaultProtocol

    bind sock (SockAddrUnix path)
    listen sock 1

    forever $ do
        (client, _) <- accept sock

        _ <- forkIO $
            finally
                (let loop = do
                        bs <- NBS.recv client 4096
                        unless (bs == mempty) $ do
                            handler (C8.unpack bs)
                            loop
                 in loop)
                (close client)

        pure ()


controlSocketWriter :: FilePath -> String -> IO ()
controlSocketWriter path msg = do
    sock <- socket AF_UNIX Stream defaultProtocol
    connect sock (SockAddrUnix path)
    _ <- NBS.send sock (C8.pack msg)
    close sock


-- ??: what's a "live event"?
-- persistMetrics :: Db.Connection -> TQueue LiveEvent -> Flag -> IO ()
-- persistMetrics conn e shutdownFlag = loop
persistMetrics :: Connection -> IO ()
persistMetrics conn = loop
-- flushTQueue :: TQueue a -> STM [a]
    where
    loop = do
        case True of
            True -> pure ()

insertRun :: Connection -> RunRow -> IO (Maybe Int64)
insertRun conn rr = do
    result <- try $ do
        execute conn
                "INSERT INTO runs (name, script_path, started_at, ended_at, status, concurrency, rate_limit, control_socket) VALUES (?, ?, unixepoch('now'), ?, ?, ?, ?, ?)"
                ("dummy" :: Text, "test.hhs" :: Text, 0 :: Int64, "completed" :: Text, 1 :: Int, 0.0 :: Double, "" :: Text)
        lastInsertRowId conn
    case result of
        Left (_ :: SomeException) -> pure Nothing
        Right rid -> pure (Just rid)

-- goStraight :: Script -> Args -> IO ()
--
-- -- Script execution.
-- -- hh200 flow.hhs
-- -- - Always touches filesystem sqlite. Non-RPS runs are saved or not saved to database.
-- goStraight script args = do
--     testSimple
--
--     where
--     testSimple :: IO ()
--     testSimple = do
--         -- let scripts = workOptimize script
--         let scripts = Tbwp.dummyDuo script
--
--
--         conn <- initDb
--         let rr = RunRow
--                 { runName = "default"
--                 -- , runScriptPath = maybe "" pack args.source
--                 , runScriptPath = ""
--                 , runStartedAt = 0
--                 , runEndedAt = ETStillRunning
--                 , runStatus = "running"
--                 , runConcurrency = length scripts
--                 , runRateLimit = 0.0
--                 , runControlSocket = ""
--                 }
--         mRunId <- insertRun conn rr
--
--         shutdownFlag <- newTVarIO False
--         doneSignals <- replicateM (length scripts) newEmptyMVar
--
--         forM_ (zip3 [1..] scripts doneSignals) $ \(i, s, done) -> do
--             let cfg = Tbwp.WorkerConfig { Tbwp.wcMode = Tbwp.OneShot
--                                         , Tbwp.wcRateLimiter = Nothing
--                                         , Tbwp.wcWorkerId = i
--                                         }
--             forkIO (Tbwp.worker cfg s shutdownFlag done)
--
--         -- ??: (callsite of persistMetrics) when there's no live events left.
--         -- Termination with ctrl+c, which is handled foremostly by worker.
--         _ <- installHandler sigINT
--                             (CatchOnce (atomically $ writeTVar shutdownFlag True))
--                             Nothing
--
--         -- Termination when all workers are done.
--         _ <- forkIO $ do
--             forM_ doneSignals readMVar
--             atomically $ writeTVar shutdownFlag True
--
--         -- Termination based on timer.
--         _ <- forkIO $ do
--             threadDelay (10 * 1000000)
--             atomically $ writeTVar shutdownFlag True
--
--         atomically (readTVar shutdownFlag >>= check)
--         -- closeDb conn


-- Globally interruptible worker(s) running Script.
-- Worker(s) are dropped after the last CallItem.

-- Concurrent one-shot: fire N workers, report how many failed.
testShotgun :: Int -> Script -> IO ()
testShotgun numWorkers script = do
    shutdownFlag <- newTVarIO False
    doneSignals <- replicateM numWorkers newEmptyMVar

    forM_ (zip [1..numWorkers] doneSignals) $ \(i, done) -> do
        let cfg = Tbwp.WorkerConfig { Tbwp.wcMode = Tbwp.OneShot, Tbwp.wcRateLimiter = Nothing, Tbwp.wcWorkerId = i }
        forkIO (Tbwp.worker cfg script shutdownFlag done)

    -- Termination with ctrl+c.
    _ <- installHandler sigINT
                        (CatchOnce (atomically $ writeTVar shutdownFlag True))
                        Nothing  -- Other signals to block.

    -- Termination based on timer.
    _ <- forkIO $ do
        threadDelay (10 * 1000000)
        atomically $ writeTVar shutdownFlag True

    -- Wait for all workers to finish.
    forM_ doneSignals takeMVar
    putStrLn $ "# testShotgun: " ++ show numWorkers ++ " workers completed."

-- Rampup-able pool of virtual users with rate limiting.
-- RPS: rate of individual CallItems.
-- ??: if the name testRps survives, find sibling i.e. as a where clause fn
testRps :: Int -> Int -> Int -> Int -> Script -> IO ()
testRps rpsVal concurrency rampUpUs thinkTimeUs script = do
    -- connect "timeseries.db"

    shutdownFlag <- newTVarIO False
    doneSignals <- replicateM concurrency newEmptyMVar

    -- Tbwp.withRateLimiter (Tbwp.RateLimiterConfig rpsVal rpsVal) $ \rl -> do
    Tbwp.withRateLimiter (undefined rpsVal rpsVal) $ \rl -> do
        -- Ramp-up: fork one VU at a time with delay between each.
        forM_ (zip [1..concurrency] doneSignals) $ \(i, done) -> do
            let cfg = Tbwp.WorkerConfig
                    { Tbwp.wcMode = Tbwp.LoopWithNap thinkTimeUs
                    , Tbwp.wcRateLimiter = Just rl
                    , Tbwp.wcWorkerId = i
                    }
            _ <- forkIO (Tbwp.worker cfg script shutdownFlag done)
            when (i < concurrency) $ threadDelay rampUpUs

        putStrLn $ "# testRps: rate=" ++ show rpsVal ++ " reqs/sec, workers=" ++ show concurrency

        -- Termination with ctrl+c.
        _ <- installHandler sigINT
                            (CatchOnce (atomically $ writeTVar shutdownFlag True))
                            Nothing  -- Other signals to block.

        -- Termination based on timer.
        _ <- forkIO $ do
            threadDelay (10 * 1000000)
            atomically $ writeTVar shutdownFlag True

        atomically (readTVar shutdownFlag >>= check)

mkArgs :: Args
mkArgs = Args { source = Nothing
              , version = False
              , debugConfig = False
              , call = False
              -- , rps = False
              , nvu = 1
              , duration = 0
              , lsp = Nothing
              , lspStdio = False
              , browse = Nothing
              }

--------------------------------------------------------------------------------
-- Dashboard server
--------------------------------------------------------------------------------
-- Path                  Notes
-- GET /api/runs         List runs from SQLite
-- GET /api/runs/live    In-progress run at the time being
-- GET /api/runs/:runId  Completed run detail
-- GET /sse              SSE stream for live inserts
-- POST /sig             Receives pause/resume/stop from dashboard, forwards to UDS
-- GET /*                SvelteKit static SPA

-- ?? full review of RunRow: runEndedAt is optional
data RREndTime =
    ETStillRunning
  | ETHasEnded !Int64

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

startServer :: String -> IO ()
startServer portStr = do
    let port = read portStr :: Int
    putStrLn $ "hh200 dashboard listening on http://localhost:" ++ show port
    scotty port $ do
        Server.middleware (staticPolicy (addBase "min"))

        Server.get "/api/runs" $ do
            conn <- liftIO initDb
            -- (auto)
            rows <- liftIO $ (query_ conn "SELECT name, script_path, started_at, ended_at, status, concurrency, rate_limit, control_socket FROM runs ORDER BY started_at DESC" :: IO [RunRow])
            Server.json $ object ["runs" .= rows]
            -- Server.json $ object []

        Server.post "/api/sig" $ do
            body <- Server.body
            liftIO $ controlSocketWriter "/tmp/uds_socket" "hello from writer"
            Server.json $ object ["ok" .= (True :: Bool)]

        Server.get (regex "(.*)") $
            Server.file "min/200.html"
