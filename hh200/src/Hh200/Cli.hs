{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Hh200.Cli
  ( cli
  , mkArgs
  , go, Args(..), optsInfo
  ) where

import Debug.Trace

import           Options.Applicative
-- import           Control.Exception (bracket, finally)
import           Control.Exception (finally)
import           Control.Monad (unless)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Maybe
import           Control.Monad (forM_, replicateM, when, forever)
import           Control.Concurrent
import           Control.Concurrent.STM
-- import           Control.Concurrent.STM.TQueue (flushTQueue)
import           System.Posix.Signals (installHandler, sigINT, Handler(CatchOnce))
import           System.Exit (exitWith, ExitCode(ExitFailure))
import           System.IO (hPutStrLn, stderr, stdout)
import qualified System.IO (hFlush)
import           System.Directory (doesFileExist)
-- import           System.Directory (removeFile, doesFileExist)
import           Data.Maybe (fromMaybe)
import           Data.Text (pack)
import           Data.Version (showVersion)
import           Database.SQLite.Simple (Connection)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL

import           Network.Socket
import qualified Network.Socket.ByteString as NBS

import qualified Paths_hh200 (version)
import qualified Hh200.TokenBucketWorkerPool as Tbwp (wcWorkerId, wcRateLimiter, wcMode, WorkerConfig(..), worker, withRateLimiter, WorkerMode(..))
-- import           Hh200.TokenBucketWorkerPool (RunState(..), worker, courier)
import           Hh200.TokenBucketWorkerPool (RunState(..), courier)
import           Hh200.Types
import qualified Hh200.Scanner as Scanner
import           Hh200.Database (initDb, insertRun, RunRow(..), RREndTime(..))
import           Hh200.LanguageServer (runTcp, runStdio)
import           Hh200.Dashboard (startServer)

-- import System.Directory   (removePathForcibly)
-- import System.IO          (BufferMode (..), Handle, IOMode (..), hClose,
--                            hSetBuffering)


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

        <*> pure Nothing)
        <*> optional ((\n a -> a { nvu = n, duration = 0 }) <$>
                      option auto ( long "shotgun"
                                 <> help "Alias for --nvu=N --duration=0"
                                 <> metavar "N" ))

-- go and goMode indirection: debug programming directly in Script structs.
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
go Args { browse = Just port } = startServer (show port)

-- Static-check script.
-- hh200 flow.hhs --debug-config
go Args { source = Just path, debugConfig = True } = do
    undefined
    -- let analyzed = Scanner.analyze path
    -- m <- runMaybeT analyzed
    -- case m of
    --     _ -> undefined

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
            Just script -> trace ("goMode path=" ++ show path) $ goMode script args
            _ -> error "bug in hh200 grammar!"

-- Inline program execution.
-- hh200 --call "GET http://localhost:9999/health"
go args@Args { call = True, source = Just snip } = do
    m <- runMaybeT (Scanner.analyze (Snippet (BL.fromStrict (C8.pack snip))))
    case m of
        Just script -> goMode script args
        _ -> error "bug in hh200 grammar!"

-- Shotgun.
-- hh200 flow.hhs --nvu=4 --duration=0
        -- Just script  -> testShotgun n script
-- Load test mode.
-- hh200 flow.hhs --duration=30
go args@Args { duration = n, call = False, source = Just path } = do
    mScript <- runMaybeT (Scanner.analyze path)
    case mScript of
        Nothing -> exitWith (ExitFailure 1)
        -- Just script  -> trace "this goMode!!!!" $ goMode script args
        Just script  -> goMode script args

-- Verifiable with `echo $?` which prints last exit code in shell.
go _ = exitWith (ExitFailure 1)

genName = "default run name"

goMode :: Script -> Args -> IO ()

goMode script args = do
    testSimple

    where
    -- terminate :: Flag -> IO ()
    terminate v = atomically $ writeTVar v Stopped

    mkRunRow :: IO RunRow
    mkRunRow = do
        pure $ RunRow
          { runId = 0
          , runControlSocket = "/tmp/uds_socket"
          , runStatus = "running"
          , runEndedAt = ETStillRunning
          -- args fields --
          , runRateLimit = 0.0
          , runScriptPath = pack $ fromMaybe "" (source args)
          -- system io --
          , runName = genName
          , runStartedAt = 0
          , runConcurrency = nvu args
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

        -- The loop that fires HTTP requests.
        -- ??: Automatic 1-second stagger for nvu >= 4. Starting point could be
        -- to assume sequencing N imperative forkIO $ courier ... statements
        -- starts N couriers at the same instant.
        let argsNvu = nvu args
        drawer <- replicateM argsNvu newEmptyMVar
        -- forM_ (zip [] drawer) $ \(_, hole) -> do
        forM_ (drawer) $ \(hole) -> do
            forkIO $ courier script (s, duration args) hole

        -- Termination with ctrl+c, which is handled foremostly by worker.
        _ <- installHandler sigINT
                            (CatchOnce $ terminate s)
                            Nothing

        -- Unix Domain Socket listener
        _ <- forkIO $
            -- controlSocketListener (runControlSocket rr) $ \msg ->  -- ??: xdg comply
            controlSocketListener "/tmp/uds_socket" $ \msg ->
                case msg of
                    "pause" ->  atomically $ writeTVar s Paused
                    "resume" -> atomically $ writeTVar s Running
                    "stop" ->   terminate s
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

        -- (auto)
        results <- mapM tryReadMVar drawer
        when (or [ b | Just b <- results ]) $
            exitWith (ExitFailure 1)


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


-- persistMetrics :: Db.Connection -> TQueue LiveEvent -> Flag -> IO ()
-- persistMetrics conn e shutdownFlag = loop
persistMetrics :: Connection -> IO ()
persistMetrics conn = loop
-- flushTQueue :: TQueue a -> STM [a]
    where
    loop = do
        case True of
            True -> pure ()

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
