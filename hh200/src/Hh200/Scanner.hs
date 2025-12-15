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
import Hh200.Types (Script(..), HostInfo(..), Snippet(..), hiHh200Conf, defaultHostInfo)
import L
import P

import qualified Data.ByteString.Lazy.Char8 as L8
import System.Directory (doesFileExist)
-- import Network.URI (parseURI, uriScheme, uriAuthority, uriPort, uriRegName) -- Unused now

import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class

import System.Process (readProcess)
import qualified System.Info as Info
import Control.Exception (try, IOException)

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

gatherHostInfo :: IO HostInfo
gatherHostInfo = do
    hn <- tryReadProcess "hostname" []
    up <- tryReadProcess "uptime" ["-p"]
    
    pure defaultHostInfo
        { hiHostname = hn
        , hiUptime = up
        , hiOs = Just Info.os
        , hiArch = Just Info.arch
        }

tryReadProcess :: FilePath -> [String] -> IO (Maybe String)
tryReadProcess cmd args = do
    result <- try (readProcess cmd args "") :: IO (Either IOException String)
    case result of
        Left _ -> pure Nothing
        Right s -> pure (Just (trim s))

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile (== ' ') . dropWhile (== '\n')

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
        hi <- liftIO gatherHostInfo
        pure (script, hi)


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
                hi <- liftIO gatherHostInfo
                let scriptWithConfig = case hiHh200Conf hi of
                                            Just sc -> baseScript { config = sc }
                                            Nothing -> baseScript
                return (Just (scriptWithConfig, hi))
