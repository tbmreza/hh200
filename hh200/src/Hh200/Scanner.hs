{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Re-export lexer and parser generated code.
module Hh200.Scanner
    ( module Hh200.Scanner
    , module L
    , module P
    ) where

import qualified Hh200.Types as Hh
import Hh200.Types
import L
import P

import qualified Data.ByteString.Lazy.Char8 as L8

import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import System.Environment (lookupEnv)

read :: String -> IO (Maybe CallItem)
read input = do
    let tokensOrPanic = alexScanTokens input
    case parse tokensOrPanic of
        ParseOk (Script { call_items = [item] }) -> do
            return (Just item)
        ParseOk _ -> do
            -- ??: log unexpected input
            return Nothing
        ParseFailed _ -> do
            return Nothing

-- Abstract syntax for downloading 2 parallel files.
seed = RequestSpec
      { verb = "GET"
      , url = ast1
      , headers = []
      , payload = ""
      , opts = []
      }
      where
      ast1 = case parse $ alexScanTokens "h" of
            ParseFailed m -> m
      -- ast :: E (Maybe Script) = case parse $ alexScanTokens "h" of
      ast = case parse $ alexScanTokens "h" of
            ParseOk d -> ParseOk d
            -- ParseFailed m -> m

rs = RequestSpec
      -- { m = "GET"
      -- , verb = "GET"
      { verb = "GET"
      -- , url = "https://fastly.picsum.photos/id/19/200/200.jpg?hmac=U8dBrPCcPP89QG1EanVOKG3qBsZwAvtCLUrfeXdE0FI"
      , url = "http://localhost:9999/lk"
      , headers = []
      , payload = ""
      , opts = []
      }
rp = ResponseSpec
      { codes = [200, 201]
      , output = ["/home/tbmreza/gh/hh200/img-.jpg"]
      }
ci = CallItem
      { ci_deps = []
      , ci_name = "download image.jpg"
      -- , ci_request_spec = rs
      , ci_request_spec = seed
      , ci_response_spec = Just rp
      }


-- -- Returns HTTP test execution recipe (user configs & user program).
-- compile1 :: FilePath -> IO (Maybe (Hh.ScriptConfig, Hh.HttpM L8.ByteString))
-- compile1 x = do
--     return Nothing

-- lookupEnv :: String -> IO (Maybe String)
iomayb :: FilePath -> IO (Maybe (Hh.HttpM L8.ByteString))
iomayb _ = return Nothing

compile :: FilePath -> IO (Hh.ScriptConfig, Hh.HttpM L8.ByteString)
compile x = do
    let Hh.Script { Hh.config = local, Hh.call_items } = preparsed x
    effective <- configsReconcile local
    return (effective, Hh.stackHh call_items)

    where
    configsReconcile :: Hh.ScriptConfig -> IO Hh.ScriptConfig
    configsReconcile local = do
        _envConfig <- return defaultScriptConfig -- ??
        return local

-- A valid program snippet is suited up as a Script of single CallItem.
flyingScript :: String -> MaybeT IO Script
flyingScript snippet = do
    user <- liftIO $ Hh200.Scanner.read snippet
    MaybeT $ case user of
        Just item ->
            return $ Just Script { config = defaultScriptConfig , call_items = [item] }
        _ ->
            -- ??: log printing effective config and defaultCallItem
            return $ Nothing



---------------------------
-- Test abstract syntax. --
---------------------------

preparsed :: String -> Hh.Script
preparsed _dummy =
  Hh.Script
    { Hh.config =
        Hh.ScriptConfig
          { Hh.retries = 0
          , Hh.max_duration = Nothing
          , Hh.subjects = [Subject "user1", Subject "user2"]
          }
    , Hh.call_items = [ci]
    }
