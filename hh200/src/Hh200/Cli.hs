{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hh200.Cli
    ( cli
    ) where

import Data.Version (showVersion)
import Control.Monad (when)
import Options.Applicative
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import System.FilePath ((</>))
import System.Exit (exitWith, ExitCode(ExitFailure))

import qualified Paths_hh200 (version)
import qualified Hh200.Types as Hh
import qualified Hh200.Fearless as Hh
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
    -- let (scriptConfig, _) = Hh.compile src
    (scriptConfig, _) <- Hh.compile src
    putMergedConfigs scriptConfig where

    putMergedConfigs :: Hh.ScriptConfig -> IO ()
    -- ??: ~/.config/hh200
    putMergedConfigs scriptConfig = putStrLn $ Hh.pp scriptConfig

-- hh200 --call "GET ..."
go Args { call = True, source = Just snippet } = do
    -- ??: rm last runHttpM
    -- maybeCallable <- Hh.read snippet
    -- case maybeCallable of
    --     Nothing -> putStrLn snippet
    --     Just ci -> Hh.runHttpM $ Hh.httpGet_ "http://localhost:9999/cli"
    res <- runMaybeT $ do
        script <- Hh.flyingScript snippet
        leads <-  Hh.testOutsideWorld script
        liftIO (putStrLn $ Hh.present leads)

    case res of
        Just _ -> do
            -- putStrLn "atasz"
            return ()
        Nothing -> do
            -- putStrLn "bwhhh"
            -- Verifiable with `echo $?` which prints last exit code in shell.
            exitWith (ExitFailure 1)

    -- return ()

-- hh200 /home/tbmreza/gh/hh200/examples/hello.hhs
go Args { call = False, source = Just path } = do
    res <- runMaybeT $ do
        script <- Hh.staticChecks path
        leads <-  Hh.testOutsideWorld script
        liftIO (putStrLn $ Hh.present leads)
    
    case res of
        Just _ -> return ()
        Nothing ->
            -- Verifiable with `echo $?` which prints last exit code in shell.
            exitWith (ExitFailure 1)


go _ = do putStrLn "oops"
