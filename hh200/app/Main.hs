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

-- import qualified Data.ByteString as S

import Lib
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

ratsFromFile :: String -> IO [String]
ratsFromFile path =  -- ??: data filepath
    return ["wardah.21", "wardah.23"]

data Ckallable = Ckallable { mkethod :: String, ukrl :: String }
    deriving Show

-- Everything a system-under-test maintainer could ask for when reproducing our
-- counter-example.
--
-- Ckallable, DNS configs (??: /etc/resolv.conf), Execution time,
data Lead = Lead { c :: Ckallable, verbose :: Bool }

instance Show Lead where
    show (Lead (Ckallable m u) v) =
        -- ??: verbose prints fields other than Ckallable
        show m ++ "\n" ++ show u ++ "\n"


-- GOAL: parallel users (instead of async top-level semantics)
-- examples/download.hhs downloads the same file twice
go :: String -> MVar Lead -> IO ()
go rat var = handle handler $ forever $ do
    putStrLn "Rat is walking the script AST..."
    threadDelay 500000

    let l = Lead
            { c = Ckallable
                  { mkethod = "GET"
                  , ukrl = "https://httpbin.org/get"
                  }
            , verbose = False
            }

    putMVar var l

    where

    handler :: AsyncException -> IO ()
    handler ThreadKilled = do
        putStrLn "Rat received ThreadKilled and is cleaning up."
    handler e = throwIO e

main :: IO ()
main = do
    -- let Mini { m_url } = P.ast1
    -- _ <- doOrder Rat m_url

    -- _ <- runHttpM prog


    var <- newEmptyMVar

    rats <- ratsFromFile ""

    tids <- forM rats $
        \rat -> forkIO (go rat var)

    firstLead <- takeMVar var
    putStrLn $ show firstLead
    forM_ tids killThread
