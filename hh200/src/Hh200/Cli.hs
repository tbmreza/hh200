{-# LANGUAGE ScopedTypeVariables #-}

module Hh200.Cli
  ( cli
  , testSimple
  , go, Args(..), optsInfo
  , go', mkArgs
  ) where

import Debug.Trace

import qualified Data.HashMap.Strict as HM
import qualified Hh200.Http as Http
import           Hh200.Graph (connect)
import           Control.Exception        (bracket, bracket_, finally, try, SomeException)
import           System.Posix.Signals     (installHandler, sigINT, Handler(CatchOnce))
import           Control.Concurrent.Async (mapConcurrently, replicateConcurrently_)

import           Control.Monad (unless)
import           Control.Monad (foldM, forM, mzero, forever, void)
import qualified Data.ByteString.Lazy.Char8 as L8
import           Control.Monad.Trans.Maybe
import           System.Exit (exitWith, ExitCode(ExitFailure))
import           System.IO (hPutStrLn, stderr, stdout)
import qualified System.IO (hFlush)
import           Options.Applicative
import           Data.Version (showVersion)
import qualified Paths_hh200 (version)
import           Hh200.Types
import           Hh200.Execution
import qualified Hh200.Scanner as Scanner
import           Hh200.LanguageServer (runTcp, runStdio)

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad (forM_, replicateM, replicateM_, when)
import qualified Hh200.TokenBucketWorkerPool as Tbwp (wcWorkerId, wcRateLimiter, wcMode, WorkerConfig(..), worker, withRateLimiter, RateLimiterConfig, dummyDuo, WorkerMode(..))


-- data Src =
--     AbstractSrc  -- for script ds
--   | PathSrc  -- "" as Nothing

data Args = Args
  { source :: Maybe String  -- used for both FilePath and Snippet sources
  , version :: Bool
  , debugConfig :: Bool
  , call :: Bool
  , rps :: Bool
  , shotgun :: Int
  , lsp :: Maybe Int
  , lspStdio :: Bool
  } deriving (Show, Eq)

cli :: IO ()
cli = go =<< execParser optsInfo

optsInfo :: ParserInfo Args
optsInfo = info (args <**> helper) (fullDesc
                                 <> header "Run hh200 scripts") where
    args = Args
        -- Bound by order, not by name; allowing e.g. different casing between
        -- above `debugConfig` and below `debug-config`.
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

        <*> switch ( long "rps"
                  <> short 'R'
                  <> help "Start \"requests per second\" mode" )

        <*> option auto ( long "shotgun"
                       <> short 'S'
                       <> help "Execute in N parallel workers at once"
                       <> metavar "N"       -- Displays as: -S N or --shotgun N
                       <> value 1           -- Default to 1 if flag is omitted
                       <> showDefault )     -- Shows "[default: 1]" in help text

        <*> optional ( option auto ( long "lsp"
                                  <> short 'd'
                                  <> help "Run hh200 language server"
                                  <> metavar "PORT" ) )

        <*> switch ( long "lsp-stdio"
                  <> help "Run hh200 language server over stdio" )

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

-- Static-check script.
-- hh200 flow.hhs --debug-config
go Args { source = Just path, debugConfig = True } = do
    let analyzed = Scanner.analyze path
    m <- runMaybeT analyzed
    case m of
        _ -> undefined
        -- ?? define debug-config

-- Script execution.
-- hh200 flow.hhs
go Args { shotgun = 1, call = False, rps = False, source = Just path } = do
    let analyzed = Scanner.analyze path
    m <- runMaybeT analyzed
    case m of
        Just script -> trace ("path=" ++ show path) $ testSimple script
        _ -> error "bug in hh200 grammar!"

-- Inline program execution.
-- hh200 --call "GET ..."
go Args { call = True, source = Just snip } =
    undefined

-- Inserts timeseries data to a file database and optionally serves a web frontend.
-- hh200 flow.hhs --rps
go Args { rps = True, shotgun = n, source = Just path } = do
    mScript <- runMaybeT (Scanner.analyze path)
    case mScript of
        Nothing -> exitWith (ExitFailure 1)
        -- Just s  -> testRps rpsVal concurrency rampUpUs thinkTimeUs script
        Just s  -> testRps 10 n 1000000 500000 s

-- Shotgun.
-- hh200 flow.hhs --shotgun=4
go Args { shotgun = n, call = False, source = Just path } = do
    mScript <- runMaybeT (Scanner.analyze path)
    case mScript of
        Nothing -> exitWith (ExitFailure 1)
        Just s  -> testShotgun n s

-- Verifiable with `echo $?` which prints last exit code in shell.
go _ = exitWith (ExitFailure 1)

go' :: Script -> Args -> IO ()

-- Script execution.
-- hh200 flow.hhs
go' script Args { shotgun = 1, call = False, rps = False } = do
    testSimple script

go' script args = trace ("go':" ++ show args) $ exitWith (ExitFailure 1)

-- Globally interruptible worker(s) running Script.
-- Worker(s) are dropped after the last CallItem.

testSimple :: Script -> IO ()
testSimple script = do
    -- let scripts = workOptimize script
    let scripts = Tbwp.dummyDuo script

    shutdownFlag <- newTVarIO False
    doneSignals <- replicateM (length scripts) newEmptyMVar

    forM_ (zip3 [1..] scripts doneSignals) $ \(i, s, done) -> do
        let cfg = Tbwp.WorkerConfig { Tbwp.wcMode = Tbwp.OneShot
                                    , Tbwp.wcRateLimiter = Nothing
                                    , Tbwp.wcWorkerId = i
                                    }
        forkIO (Tbwp.worker cfg s shutdownFlag done)

    -- Termination with ctrl+c, which is handled foremostly by worker.
    _ <- installHandler sigINT
                        (CatchOnce (atomically $ writeTVar shutdownFlag True))
                        Nothing

    -- Termination when all workers are done.
    _ <- forkIO $ do
        forM_ doneSignals readMVar
        atomically $ writeTVar shutdownFlag True

    -- Termination based on timer.
    _ <- forkIO $ do
        threadDelay (10 * 1000000)
        atomically $ writeTVar shutdownFlag True

    atomically (readTVar shutdownFlag >>= check)

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
testRps :: Int -> Int -> Int -> Int -> Script -> IO ()
testRps rpsVal concurrency rampUpUs thinkTimeUs script = do
    connect "timeseries.db"  -- ??:

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
            forkIO (Tbwp.worker cfg script shutdownFlag done)
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
              , rps = False
              , shotgun = 1
              , lsp = Nothing
              , lspStdio = False
              }
