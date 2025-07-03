{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Hh200.Cli

-- GOAL: conveniently present first counter-example

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Control.Monad.Reader

import Network.HTTP.Client

import Hh200.Types

import qualified Data.ByteString.Lazy.Char8 as L8

import P  -- ??: concrete syntax

prog :: HttpM ()
prog = do
    json <- httpGet "https://httpbin.org/json"
    liftIO $ putStrLn $ "GET response: " ++ take 100 (L8.unpack json)
    -- postResp <- httpPost "https://httpbin.org/post" "{\"reader\": \"monad\"}"
    -- liftIO $ putStrLn $ "POST response: " ++ take 100 (L8.unpack postResp)

-- These rats compete with each other for the first counter-example or `Lead`.
-- At the end of the race, all rats die; "rat race".

main :: IO ()
main = cli
