{-# LANGUAGE NamedFieldPuns #-}

module Hh200.Cli
    ( cli
    ) where

import Data.Version (showVersion)
import Control.Monad (when)
import Options.Applicative

import qualified Paths_hh200 (version)
import qualified Hh200.Types as Hh
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
    -- let (scriptConfig, _) = Hh.fromHhs src
    (scriptConfig, _) <- Hh.fromHhs src
    putMergedConfigs scriptConfig where

    putMergedConfigs :: Hh.ScriptConfig -> IO ()
    -- ??: ~/.config/hh200
    putMergedConfigs scriptConfig = putStrLn $ Hh.pp scriptConfig

-- hh200 --call "GET ..."
go Args { call = True, source = Just snippet } = do
    maybeCallable <- Hh.read snippet
    case maybeCallable of
        Nothing -> putStrLn snippet
        Just ci -> Hh.runHttpM $ Hh.httpGet_ "http://localhost:9999/you"

-- two rats downloading the same image
-- "download image.jpg"
-- GET https://fastly.picsum.photos/id/19/200/200.jpg?hmac=U8dBrPCcPP89QG1EanVOKG3qBsZwAvtCLUrfeXdE0FI
-- HTTP [200 201] ("/home/tbmreza/gh/hh200/hh200/img-{{row.username}}.jpg" fresh)
-- go Args { call = True, debugConfig = False } = do
-- concurrency: empty deps call items
-- go Args { call = True } = do
--     let out = Hh.pp Hh.defaultCallItem
--     putStrLn out
--     -- Hh.raceToLead Hh.ast2images  -- ok

-- hh200 /home/tbmreza/gh/hh200/examples/hello.hhs
go Args { call = False, source = Just path } = do
    -- let (effectiveCfg, ret) = Hh.fromHhs path
    (effectiveCfg, ret) <- Hh.fromHhs path

    let isVerbose = True -- ??: cli arg
    when isVerbose $
        putStrLn (show effectiveCfg)

    Hh.runHttpM $ ret
    -- return ()

go _ = do putStrLn "oops"
