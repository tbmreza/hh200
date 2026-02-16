{-# LANGUAGE ScopedTypeVariables #-}

module Hh200.Cli
  ( cli
  -- Exported for testing:
  , go, Args(..), optsInfo
  ) where

import Debug.Trace

import qualified Data.HashMap.Strict as HM
import qualified Hh200.Http as Http
import           Hh200.Graph (connect)
import           Control.Exception        (bracket, bracket_, try, SomeException)
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
import           Hh200.LanguageServer (runTcp)

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad (forM_, replicateM, replicateM_, when)
import           Hh200.TokenBucketWorkerPool

data Args = Args
  { source :: Maybe String  -- used for both FilePath and Snippet sources
  , version :: Bool
  , debugConfig :: Bool
  , call :: Bool
  , rps :: Bool
  , shotgun :: Int
  , lsp :: Maybe Int
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

go :: Args -> IO ()

-- Print executable version.
-- hh200 --version
go Args { version = True } = do
    putStrLn $ showVersion Paths_hh200.version
    System.IO.hFlush stdout

-- Run language server.
-- hh200 --lsp=3000
go Args { lsp = Just port } = runTcp port

-- Static-check script.
-- hh200 flow.hhs --debug-config
go Args { source = Just path, debugConfig = True } = do
    let analyzed = Scanner.analyze path
    m <- runMaybeT analyzed
    case m of
        Just script -> trace "here" $ testSimple script
        _ -> undefined

-- Script execution.
-- hh200 flow.hhs
go Args { shotgun = 1, call = False, rps = False, source = Just path } =
    runAnalyzedScript (Scanner.analyze path)

-- Inline program execution.
-- hh200 --call "GET ..."
go Args { call = True, source = Just snip } =
    runAnalyzedScript (Scanner.analyze (Snippet $ L8.pack snip))

-- Inserts timeseries data to a file database and optionally serves a web frontend.
-- hh200 flow.hhs --rps
go Args { rps = True, shotgun = n, source = Just path } = do
    mScript <- runMaybeT (Scanner.analyze path)
    case mScript of
        Nothing -> exitWith (ExitFailure 1)
        -- Just s  -> testRps 10 n s
        Just s  -> undefined

-- Shotgun.
-- hh200 flow.hhs --shotgun=4
go Args { shotgun = n, call = False, source = Just path } = do
    mScript <- runMaybeT (Scanner.analyze path)
    case mScript of
        Nothing -> exitWith (ExitFailure 1)
        Just s  -> do
            undefined
            -- testShotgun n s
            putStrLn "Shotgun test complete."

-- Verifiable with `echo $?` which prints last exit code in shell.
go _ = exitWith (ExitFailure 1)

-- Globally interruptible worker(s) running Script.
testSimple :: Script -> IO ()
testSimple script = do
    -- ??: Script/CallItems can be analyzed to determine the number of workers testSimple can use.
    -- A sanity check can assert that if we lowball the Script just takes longer to run.
    -- let orthogonal = [VUState { workerId = 1 }, VUState { workerId = 2 }, VUState { workerId = 3 }]
    -- let orthogonal = [VUState { workerId = 1 }, VUState { workerId = 2 }]
    let orthogonal = [VUState { workerId = 1 }]
    let numWorkers = length orthogonal

    bucket <-         newTVarIO 1000  -- ??
    globalShutdown <- newTVarIO False
    chan <-           newTChanIO  -- stream of Scripts (each containing one or more CallItem)
    doneSignals <-    replicateM numWorkers newEmptyMVar

    -- Spawn the workers
    forM_ (zip orthogonal doneSignals) $ \(vu, sig) ->
        forkIO (worker vu chan (bucket, globalShutdown) sig)

    -- Just before printing a Lead, other workers exit.

    trace "was run in cli" $ forM_ doneSignals takeMVar

-- Unminuted mode: a web service that listens to sigs for stopping hh200 from making calls.
-- RPS: rate of individual CallItems
testRps :: Int -> Int -> Script -> IO ()
testRps rpsVal concurrency script = do
    -- The web server is lazy: no start if no row is inserted to db.
    -- Inserts every second (or to a second-windowed timeseries data).
    connect "timeseries.db"

    bracket (Http.newManager (effectiveTls script))
            Http.closeManager $
            \mgr -> do
                -- rl <- initRateLimiter (RateLimiterConfig rpsVal rpsVal)
                -- let ctx = ExecContext mgr (Just rl)
                let ctx = ExecContext mgr
                putStrLn $ "# testRps: rate=" ++ show rpsVal ++ " reqs/sec, workers=" ++ show concurrency
                replicateConcurrently_ concurrency $ forever $ do
                    void $ runProcM script ctx HM.empty

testShotgun :: Int -> Script -> IO ()
testShotgun numWorkers script = do
    -- bracket (Http.newManager (effectiveTls script))
    --         Http.closeManager $
    --         \mgr -> do
    --             putStrLn $ "# testShotgun: numWorkers=" ++ show numWorkers
    --             -- _ <- mapConcurrentlyBounded numWorkers $ replicate numWorkers (runProcM script (ExecContext mgr Nothing) HM.empty)
    --             _ <- mapConcurrentlyBounded numWorkers $ replicate numWorkers (runProcM script (ExecContext mgr) HM.empty)
    --             pure ()
    --
    -- where
    -- -- Limit concurrency with QSemN
    -- mapConcurrentlyBounded :: Int -> [IO a] -> IO [a]
    -- mapConcurrentlyBounded numWorkers actions = do
    --     sem <- newQSemN numWorkers
    --     mapConcurrently
    --         (\act -> bracket_ (waitQSemN sem 1)
    --                           (signalQSemN sem 1)
    --                           act)
    --         actions
    globalShutdown <- newTVarIO False
    undefined

runAnalyzedScript :: MaybeT IO Script -> IO ()
runAnalyzedScript mis = do
    mScript <- runMaybeT mis

    script <- case mScript of
        Nothing -> exitWith (ExitFailure 1)
        Just s  -> pure s

    -- lead <- testOutsideWorld script
    --
    -- case firstFailing lead of
    --     Nothing -> do
    --         putStrLn "Script executed successfully."
    --     Just ci -> do
    --         if ciName ci == "default"
    --             then hPutStrLn stderr "hh200 encountered a system error."
    --             else do
    --                 putStrLn $ present ci
    --                 hPutStrLn stderr "hh200 found an unmet expectation."
    --         exitWith (ExitFailure 1)
    undefined
