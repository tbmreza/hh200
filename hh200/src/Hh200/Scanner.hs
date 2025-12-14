{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Re-export lexer and parser generated code.
module Hh200.Scanner
    ( module Hh200.Scanner
    , module L
    , module P
    ) where

import Debug.Trace
import Hh200.Types (Script(..), HostInfo(..), Snippet(..), CallItem(..), RequestSpec(..), url, hiHh200Conf, defaultHostInfo)
import L
import P

import qualified Data.ByteString.Lazy.Char8 as L8
import System.Directory (doesFileExist)
import Network.URI (parseURI, uriScheme, uriAuthority, uriPort, uriRegName)

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
            -- putStrLn "\t SCANNER:"
            -- putStrLn $ show tokensOrPanic
            pure $ Just s

gatherHostInfo :: Script -> HostInfo
gatherHostInfo script =
    let mUrl = case callItems script of
                (ci:_) -> Just (url (ciRequestSpec ci))
                _      -> Nothing
        (host, port, tls) = case mUrl >>= parseURI of
            Just uri ->
                let scheme = uriScheme uri
                    isTls = if scheme == "https:" then Just True else if scheme == "http:" then Just False else Nothing
                    mAuth = uriAuthority uri
                in case mAuth of
                    Just auth -> (Just (uriRegName auth), readPort (uriPort auth), isTls)
                    Nothing   -> (Nothing, Nothing, isTls)
            Nothing -> (Nothing, Nothing, Nothing)
    in defaultHostInfo { hiHost = host, hiPort = port, hiTls = tls }
    where
        readPort :: String -> Maybe Int
        readPort (':':ps) = case reads ps of
                                [(p, "")] -> Just p
                                _         -> Nothing
        readPort _        = Nothing

class Analyze a where
    analyze :: a -> MaybeT IO Script
    analyzeWithHostInfo :: a -> MaybeT IO (Script, HostInfo)


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

    analyzeWithHostInfo :: FilePath -> MaybeT IO (Script, HostInfo)
    analyzeWithHostInfo path = do
        script <- analyze path
        pure (script, gatherHostInfo script)


instance Analyze Snippet where
    -- -> Nothing | SoleScript
    analyze :: Snippet -> MaybeT IO Script
    analyze s@(Snippet _) = do
        (script, _) <- analyzeWithHostInfo s
        pure script

    analyzeWithHostInfo :: Snippet -> MaybeT IO (Script, HostInfo)
    analyzeWithHostInfo s@(Snippet _) = do
        let opt :: Maybe Script = Hh200.Scanner.scriptFrom s

        MaybeT $ case opt of
            Nothing -> do
                trace "analyzed Nothing" (return Nothing)
            Just baseScript -> do
                let hi = gatherHostInfo baseScript
                let scriptWithConfig = case hiHh200Conf hi of
                                            Just sc -> baseScript { config = sc }
                                            Nothing -> baseScript
                return (Just (scriptWithConfig, hi))
