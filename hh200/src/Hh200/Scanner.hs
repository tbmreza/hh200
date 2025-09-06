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

import Debug.Trace
import Hh200.Types
import L
import P

import qualified Data.ByteString.Lazy.Char8 as L8
import System.Directory (doesFileExist)

import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class

scriptFrom :: Snippet -> Maybe Script
scriptFrom (Snippet s) =
    let tokensOrPanic = alexScanTokens (L8.unpack s) in
    let parsed :: E Script = parse tokensOrPanic in

    case parsed of
        ParseFailed d -> trace (show d) Nothing
        ParseOk sole -> Just sole

readScript :: FilePath -> IO (Maybe Script)
readScript path = do
    loaded <- readFile path
    let tokensOrPanic = alexScanTokens loaded
    let parsed :: E Script = parse tokensOrPanic

    case parsed of
        ParseFailed m -> do
            putStrLn $ show m
            pure Nothing
        ParseOk s -> do
            putStrLn "\t SCANNER:"
            putStrLn $ show tokensOrPanic
            pure $ Just s


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
            False -> do
                -- Proceeding to testOutsideWorld is unnecessary.
                pure Nothing
            _ -> do
                readScript path

instance Analyze Snippet where
    -- -> Nothing | SoleScript
    analyze :: Snippet -> MaybeT IO Script
    analyze s@(Snippet _) = do
        let opt :: Maybe Script = Hh200.Scanner.scriptFrom s

        -- liftIO (putStrLn "casing Snippet::analyze....")

        MaybeT $ case opt of
            Nothing -> do
                trace "analyzed Nothing" (return Nothing)
            Just baseScript -> do
                hi <- gatherHostInfo
                let scOpt :: Maybe ScriptConfig = hiHh200Conf hi
                return (Just $ soleScript baseScript scOpt)

        where
        soleScript :: Script -> Maybe ScriptConfig -> Script
        soleScript base _ =
            let effective = config base in  -- ??: https://httpie.io/docs/cli/configurable-options
            let build :: Script = base
                  { config = effective
                  } in
            build
