{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hh200.Cli (cli) where

import Debug.Trace
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import System.Exit (exitWith, ExitCode(ExitFailure))
import System.Directory (doesFileExist)
import Options.Applicative
import qualified Data.ByteString.Lazy.Char8 as L8

import Data.Version (showVersion)
import qualified Paths_hh200 (version)

import Hh200.Types
import qualified Hh200.Scanner as Scanner

data Args = Args
    { source  :: Maybe String
    , version :: Bool
    , debugConfig :: Bool
    , call :: Bool
    }

cli :: IO ()
cli = go =<< execParser opts where
    opts = info (args <**> helper) (fullDesc
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
                  <> help "delete" )

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
            putStrLn "pretty"
    -- ??: ~/.config/hh200

-- Inline program execution.
-- hh200 --call "GET ..."
go Args { call = True, source = Just src } = do
    ret :: Maybe Script <- runMaybeT $ do
        script <- Scanner.analyze (Snippet $ L8.pack src)
        liftIO (return script)

    case ret of
        Nothing -> do
            trace "sole exit" (exitWith (ExitFailure 1))
        Just s -> do
            lead <- testOutsideWorld s
            putStrLn $ present lead

-- Basic script execution.
-- hh200 flow.hhs
go Args { call = False, source = Just path } = do
    ret :: Maybe Script <- runMaybeT $ do
        script <- Scanner.analyze path
        liftIO (return script)

    case ret of
        Nothing -> do
            exitWith (ExitFailure 1)
        Just s -> do
            lead <- testOutsideWorld s
            putStrLn $ present lead

go _ =
    -- Verifiable with `echo $?` which prints last exit code in shell.
    exitWith (ExitFailure 1)
