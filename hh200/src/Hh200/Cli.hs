{-# LANGUAGE ScopedTypeVariables #-}

module Hh200.Cli
  ( cli
  -- Exported for testing:
  , go, Args(..)
  ) where

import Debug.Trace

import qualified Data.ByteString.Lazy.Char8 as L8
import           Control.Monad.Trans.Maybe
import           Control.Monad.IO.Class
import           System.Exit (exitWith, ExitCode(ExitFailure))
import           System.Directory (doesFileExist)
import           System.IO (hPutStrLn, stderr)
import           Options.Applicative
import           Data.Version (showVersion)
import qualified Paths_hh200 (version)
import           Hh200.Types
import           Hh200.Execution
import qualified Hh200.Scanner as Scanner

data Args = Args
  { source  :: Maybe String
  , version :: Bool
  , debugConfig :: Bool
  , call :: Bool
  , shotgun :: Int
  }

cli :: IO ()
cli = go =<< execParser options where
    options = info (args <**> helper) (fullDesc
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
                  <> help "Read environment and script header to determine the config values without executing script's side-effects." )

        <*> switch ( long "call"
                  <> short 'C'
                  <> help "Execute a script snippet directly" )

        <*> option auto ( long "shotgun"
                       <> short 'S'
                       <> help "Execute in N parallel workers at once"
                       <> metavar "N"       -- Displays as: -S N or --shotgun N
                       <> value 1           -- Default to 1 if flag is omitted
                       <> showDefault )     -- Shows "[default: 1]" in help text


go :: Args -> IO ()

-- Print executable version.
-- hh200 --version
go Args { version = True } = putStrLn $ showVersion Paths_hh200.version

-- Static-check script.
-- hh200 flow.hhs --debug-config
go Args { source = Just src, debugConfig = True } = do
    exists <- doesFileExist src
    case exists of
        False -> exitWith (ExitFailure 1)
        _ -> do
            analyzed :: Maybe Script <- runMaybeT $ do
                script <- Scanner.analyze src
                liftIO (pure script)
            case analyzed of
                Nothing -> exitWith (ExitFailure 1)
                Just s -> print (config s)

-- Inline program execution.
-- hh200 --call "GET ..."
go Args { call = True, source = Just src } = do
    analyzed :: Maybe Script <- runMaybeT $ do
        script <- Scanner.analyze (Snippet $ L8.pack src)
        liftIO (pure script)

    case analyzed of
        Nothing -> exitWith (ExitFailure 1)
        Just s -> do
            lead <- testOutsideWorld s
            case noNews lead of
                True -> pure ()
                _ -> do
                    putStrLn $ case firstFailing lead of
                        Nothing -> "internal error"
                        Just ci -> present ci
                    hPutStrLn stderr "hh200 found an unmet expectation."
                    exitWith (ExitFailure 1)

-- Shotgun.
-- hh200 flow.hhs --shotgun=4
go Args { shotgun = n, call = False, source = Just path } = do
    -- Check if output/o.dat has # x y value header and maybe first data row. Number of bullets also could be a sanity check for something.
    -- let s = Script { config = defaultScriptConfig, callItems = [] }
    -- dp <- testShotgun n s
    analyzed :: Maybe Script <- runMaybeT $ do
        script <- Scanner.analyze path
        liftIO (pure script)

    -- ??: with DataPoint extraction and plotting flow in cli

    let dat = Dat []
    case analyzed of
        Nothing -> exitWith (ExitFailure 1)
        Just s -> do
            dp <- testShotgun n s

            pure ()
    -- Rewrites output/o.dat at the end.

-- Script execution.
-- hh200 flow.hhs
go Args { call = False, source = Just path } = do
    analyzed :: Maybe Script <- runMaybeT $ do
        script <- Scanner.analyze path
        liftIO (pure script)

    case analyzed of
        Nothing -> exitWith (ExitFailure 1)
        Just s -> do
            lead <- testOutsideWorld s
            case noNews lead of
                True ->
                    -- No news is good news.
                    pure ()
                _ -> do
                    putStrLn $ case firstFailing lead of
                        Nothing -> "internal error"
                        Just ci -> present ci
                    hPutStrLn stderr "hh200 found an unmet expectation."
                    exitWith (ExitFailure 1)

-- Verifiable with `echo $?` which prints last exit code in shell.
go _ = exitWith (ExitFailure 1)
