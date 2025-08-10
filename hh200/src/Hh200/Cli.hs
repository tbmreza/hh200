{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hh200.Cli
    ( cli
    ) where

import Data.Version (showVersion)
-- import Control.Monad (when)

import Options.Applicative
-- import qualified Options.Applicative as OA (short)
-- import Options.Applicative (help, long, switch, str, argument, metavar, optional, header, fullDesc, helper, (<**>), execParser, info)

import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
-- import System.FilePath ((</>))
import System.Exit (exitWith, ExitCode(ExitFailure))
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as L8

import qualified Paths_hh200 (version)
import qualified Hh200.Types as Hh
-- import qualified Hh200.Fearless as Hh
import qualified Hh200.Scanner as Hh

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

-- hh200 --version
go Args { version = True } = putStrLn $ showVersion Paths_hh200.version

-- hh200 flow.hhs --debug-config
go Args { source = Just src, debugConfig = True } = do
    return ()
    -- (scriptConfig, _) <- Hh.compile src
    -- putMergedConfigs scriptConfig where
    --
    -- putMergedConfigs :: Hh.ScriptConfig -> IO ()
    -- -- ??: ~/.config/hh200
    -- putMergedConfigs scriptConfig = putStrLn $ Hh.pp scriptConfig

-- hh200 --call "GET ..."
go Args { call = True, source = Just snippet } = do
    -- putStrLn $ show (L8.pack snippet)
    -- ??: source = Just s, isFilePath
    ret :: Maybe Hh.Lead <- runMaybeT $ do
        liftIO $ putStrLn "enter"
        script <- Hh.analyze $ Hh.Snippet (L8.pack snippet)  -- ParserException | Nothing(empty call items): print hostLead
        liftIO $ putStrLn "enter 2"
        leads <- Hh.testOutsideWorld script                  -- ThreadException | Nothing(config-skipped): print config
        liftIO $ putStrLn "enter 3"
        liftIO $ return leads

        -- liftIO (putStrLn "")

    case ret of
        Nothing -> do
            -- Verifiable with `echo $?` which prints last exit code in shell.
            exitWith (ExitFailure 1)
        Just l -> do
            putStrLn $ Hh.present l

-- hh200 /home/tbmreza/gh/hh200/examples/hello.hhs
go Args { call = False, source = Just path } = do
    ret :: Maybe Hh.Lead <- runMaybeT $ do
        script <- Hh.analyze path            -- ParserException | Nothing(empty call items): print hostLead
        leads <- Hh.testOutsideWorld script  -- ThreadException | Nothing(config-skipped): print config
        liftIO $ return leads

        -- liftIO (putStrLn "")

    case ret of
        Nothing -> do
            -- Verifiable with `echo $?` which prints last exit code in shell.
            exitWith (ExitFailure 1)
        Just l -> do
            putStrLn $ Hh.present l


go _ = do putStrLn "oops"
