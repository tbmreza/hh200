-- logger <- liftIO $ newStdoutLoggerSet defaultBufSize  -- ??: internal logger without passing around instance
{-# LANGUAGE InstanceSigs #-}
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

import Hh200.Types
import L
import P

import qualified Data.ByteString.Lazy.Char8 as L8
import System.Directory (doesFileExist)

import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
-- import System.Environment (lookupEnv)
-- import System.Log.FastLogger (newStdoutLoggerSet, defaultBufSize)
-- import qualified Data.ByteString.Char8 as BS

scriptFrom :: Snippet -> Maybe Script
scriptFrom (Snippet s) =
    let tokensOrPanic = alexScanTokens (L8.unpack s) in
    let parsed :: E Script = parse tokensOrPanic in

    case parsed of
        ParseFailed _ -> Nothing
        ParseOk sole -> Just sole

readScript :: FilePath -> IO (Maybe Script)
readScript path = do
    -- loaded :: BS.ByteString <- BS.readFile path  -- ??: ByteString
    loaded <- readFile path
    let tokensOrPanic = alexScanTokens loaded
    let parsed :: E Script = parse tokensOrPanic

    -- ??: upgrade ghc to write or-pattern https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0522-or-patterns.rst
    -- ParseOk, but call_items is empty array. -> StaticScript
    -- ParseOk, but call_items length is 1. -> SoleScript
    -- ParseOk. -> Script
    return $ case parsed of
        ParseFailed _ ->
            Nothing

        ParseOk s -> Just s


-- -- Returning Nothing short-circuits callsite.
-- staticChecks :: FilePath -> MaybeT IO Script
-- staticChecks path = do
--   exists <- liftIO $ doesFileExist path
--   if exists
--     then liftIO $ return defaultScript
--     else MaybeT $ return Nothing

class Analyze a where
    analyze :: a -> MaybeT IO Script

gatherHostInfo :: IO HostInfo
gatherHostInfo = do
    return defaultHostInfo

instance Analyze FilePath where
    -- -> Nothing | StaticScript | SoleScript? | Script
    analyze :: FilePath -> MaybeT IO Script
    analyze path = do
        exists <- liftIO $ doesFileExist path
        MaybeT $ case exists of
            -- Proceeding to testOutsideWorld is unnecessary.
            False -> return Nothing
            _ -> readScript path

instance Analyze Snippet where
    -- -> Nothing | SoleScript
    analyze :: Snippet -> MaybeT IO Script
    analyze s@(Snippet _) = do
        let opt :: Maybe Script = Hh200.Scanner.scriptFrom s

        -- liftIO (putStrLn "casing Snippet::analyze....")

        MaybeT $ case opt of
            Nothing -> do
                return Nothing
            Just baseScript -> do
                -- liftIO (putStrLn "some...")
                hi <- gatherHostInfo
                let scOpt :: Maybe ScriptConfig = hiHh200Conf hi
                return (Just $ soleScript baseScript scOpt)

        where
        soleScript :: Script -> Maybe ScriptConfig -> Script
        soleScript base _ =
            let effective = config base in  -- ??
            let build :: Script = base
                  { config = effective
                  } in
            build

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
