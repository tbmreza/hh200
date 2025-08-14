-- logger <- liftIO $ newStdoutLoggerSet defaultBufSize  -- ??: internal logger without passing around instance
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
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
import System.Directory (doesFileExist)

import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import System.Environment (lookupEnv)
import System.Log.FastLogger (newStdoutLoggerSet, defaultBufSize)
import qualified Data.ByteString.Char8 as BS

-- Read user input expecting valid program.
readUser :: String -> IO (Maybe CallItem)
readUser input = do
    let tokensOrPanic = alexScanTokens input
    putStrLn $ show tokensOrPanic
    case parse tokensOrPanic of
        -- ParseOk (Script { call_items = [item] }) -> do
        --     return (Just item)
        ParseOk _ -> do
            return Nothing
        ParseFailed _ -> do
            putStrLn "long way....."
            return Nothing

-- A valid program snippet is suited up as a Script of single CallItem.
flyingScript :: String -> MaybeT IO Script
flyingScript snippet = do
    user <- liftIO $ Hh200.Scanner.readUser snippet
    MaybeT $ case user of
        Just item ->
            return $ Just Script { config = defaultScriptConfig , call_items = [item] }
        _ ->
            return $ Nothing

-- Returning Nothing short-circuits callsite.
staticChecks :: FilePath -> MaybeT IO Script
staticChecks path = do
  exists <- liftIO $ doesFileExist path
  if exists
    then liftIO $ return defaultScript
    else MaybeT $ return Nothing

newtype Snippet = Snippet L8.ByteString

class Analyze a where
    analyze :: a -> MaybeT IO Script

-- Reconcile configs 

instance Analyze FilePath where
    -- Rule out empty call items as there's nothing left to be
    -- done by testOutsideWorld.
    analyze path = do
      bs <- liftIO $ BS.readFile path
      exists <- liftIO $ doesFileExist path
      if exists
        then liftIO $ return defaultScript
        else MaybeT $ return Nothing

instance Analyze Snippet where
    analyze (Snippet snippet) = do
        user1 :: Maybe CallItem <- liftIO $ Hh200.Scanner.readUser (L8.unpack snippet)
        -- liftIO $ putStrLn
        let user = Just defaultCallItem
        MaybeT $ case user of
            Just item -> do
                liftIO $ putStrLn "anal A"
                return $ Just Script { config = defaultScriptConfig , call_items = [item] }
            _ -> do
                liftIO $ putStrLn "anal B"
                return $ Nothing

instance Analyze L8.ByteString where
-- instance Analyze Snippet where
    -- defaultCallItem doesn't make sense when statically analyzing user's call_item.
    analyze snippet = do
        user <- liftIO $ Hh200.Scanner.readUser (L8.unpack snippet)
        MaybeT $ case user of
            Just item ->
                -- return (Just $ soleScriptItem item)  -- ??
                return $ Just Script { config = defaultScriptConfig , call_items = [item] }
            _ ->
                return $ Nothing

---------------------------
-- Test abstract syntax. --
---------------------------
-- seed = RequestSpec
--       { verb = "GET"
--       , url = ast1
--       , headers = []
--       , payload = ""
--       , opts = []
--       }
--       where
--       ast1 = case parse $ alexScanTokens "h" of
--             ParseFailed m -> m
--       -- ast :: E (Maybe Script) = case parse $ alexScanTokens "h" of
--       ast = case parse $ alexScanTokens "h" of
--             ParseOk d -> ParseOk d
--             -- ParseFailed m -> m
--
-- rs = RequestSpec
--       -- { m = "GET"
--       -- , verb = "GET"
--       { verb = "GET"
--       -- , url = "https://fastly.picsum.photos/id/19/200/200.jpg?hmac=U8dBrPCcPP89QG1EanVOKG3qBsZwAvtCLUrfeXdE0FI"
--       , url = "http://localhost:9999/lk"
--       , headers = []
--       , payload = ""
--       , opts = []
--       }
-- rp = ResponseSpec
--       { codes = [200, 201]
--       , output = ["/home/tbmreza/gh/hh200/img-.jpg"]
--       }
-- ci = CallItem
--       { ci_deps = []
--       , ci_name = "download image.jpg"
--       -- , ci_request_spec = rs
--       , ci_request_spec = seed
--       , ci_response_spec = Just rp
--       }



-- preparsed :: String -> Hh.Script
-- preparsed _dummy =
--   Hh.Script
--     { Hh.config =
--         Hh.ScriptConfig
--           { Hh.retries = 0
--           , Hh.max_duration = Nothing
--           , Hh.subjects = [Subject "user1", Subject "user2"]
--           }
--     , Hh.call_items = [ci]
--     }
