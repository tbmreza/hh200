{-# LANGUAGE ScopedTypeVariables #-}

module Hh200.Cli
  ( cli
  -- Exported for testing:
  , go, Args(..)
  ) where

import Debug.Trace

import Control.Monad (unless)
import Data.Maybe (fromMaybe)

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
  , rps :: Bool
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

        <*> switch ( long "rps"
                  <> short 'R'
                  <> help "Start \"requests per second\" mode" )

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

-- Inserts timeseries data to a file database and optionally serves a web frontend.
-- hh200 flow.hhs --rps
go Args { rps = True, source = Just path } = do
    mScript <- runMaybeT (Scanner.analyze path)

    script <- case mScript of
        Nothing -> exitWith (ExitFailure 1)
        Just s  -> pure s

    testRps script
    
-- Script execution.
-- hh200 flow.hhs
go Args { shotgun = 1, call = False, source = Just path } = do
    mScript <- runMaybeT (Scanner.analyze path)

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

-- Shotgun.
-- hh200 flow.hhs --shotgun=4
go Args { shotgun = n, call = False, source = Just path } = do
    mScript <- runMaybeT (Scanner.analyze path)
    
    script <- case mScript of
        Nothing -> exitWith (ExitFailure 1)
        Just s  -> pure s

    lead <- testShotgun n script

    -- ??: with DataPoint extraction and plotting flow in cli

    pure ()

    -- let dat = Dat []
    -- -- case analyzed of
    -- case Nothing of
    --     Nothing -> exitWith (ExitFailure 1)
    --     Just checked -> do
    --         lead <- testShotgun n checked
    --         pure ()

    -- Rewrites output/o.dat at the end.

-- Verifiable with `echo $?` which prints last exit code in shell.
go _ = exitWith (ExitFailure 1)
