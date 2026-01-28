{-# LANGUAGE ScopedTypeVariables #-}

module Hh200.Cli
  ( cli
  -- Exported for testing:
  , go, Args(..), optsInfo
  ) where


import Debug.Trace

import Control.Monad (unless)

import qualified Data.ByteString.Lazy.Char8 as L8
import           Control.Monad.Trans.Maybe
import           Control.Monad.IO.Class
import           System.Exit (exitWith, ExitCode(ExitFailure))
import           System.Directory (doesFileExist)
import           System.IO (hPutStrLn, stderr, stdout)
import qualified System.IO (hFlush)
import           Options.Applicative
import           Data.Version (showVersion)
import qualified Paths_hh200 (version)
import           Hh200.Types
import           Hh200.Execution
import qualified Hh200.Scanner as Scanner
import           Hh200.LanguageServer (runTcp)

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
    runAnalyzedScriptg (Scanner.analyzeg path)

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
go Args { rps = True, source = Just path } = do
    mScript <- runMaybeT (Scanner.analyze path)

    script <- case mScript of
        Nothing -> exitWith (ExitFailure 1)
        Just s  -> pure s

    -- Unminuted mode.
    testRps script

-- Shotgun.
-- hh200 flow.hhs --shotgun=4
go Args { shotgun = n, call = False, source = Just path } = do
    mScript <- runMaybeT (Scanner.analyze path)

    script <- case mScript of
        Nothing -> exitWith (ExitFailure 1)
        Just s  -> pure s

    _lead <- testShotgun n script

    -- ?? : with DataPoint extraction and plotting flow in cli

    exitWith (ExitFailure 1)

    -- Rewrites output/o.dat at the end.

-- Verifiable with `echo $?` which prints last exit code in shell.
go _ = exitWith (ExitFailure 1)


runAnalyzedScriptg :: MaybeT IO Scriptg -> IO ()
runAnalyzedScriptg mis = do
    mScript <- runMaybeT mis

    script <- case mScript of
        Nothing -> exitWith (ExitFailure 1)
        Just s  -> pure s

    lead <- testOutsideWorldg script

    -- No news is good news, otherwise:
    -- unless (noNews lead) $ do  -- ??
    unless False $ do
        putStrLn $ case gfirstFailing lead of
            -- The third and final step of hh200 (presentation).
            Just ci -> presentg ci
            -- Expect no interesting news other than first failing CallItem.
            Nothing -> undefined

        hPutStrLn stderr "hh200 found an unmet expectation."
        exitWith (ExitFailure 1)

runAnalyzedScript :: MaybeT IO Script -> IO ()
runAnalyzedScript mis = do
    mScript <- runMaybeT mis

    script <- case mScript of
        Nothing -> exitWith (ExitFailure 1)
        Just s  -> pure s

    lead <- testOutsideWorld script

    -- No news is good news, otherwise:
    unless (noNews lead) $ do
        putStrLn $ case firstFailing lead of
            -- The third and final step of hh200 (presentation).
            Just ci -> present ci
            -- Expect no interesting news other than first failing CallItem.
            Nothing -> undefined

        hPutStrLn stderr "hh200 found an unmet expectation."
        exitWith (ExitFailure 1)
