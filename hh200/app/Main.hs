module Main (main) where

import Hh200.Cli

-- main :: IO ()
-- main = cli

-- GOAL: conveniently present first counter-example

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Lib


-- PICKUP import P

-- These rats compete with each other for the first counter-example or `Lead`.
-- At the end of the race, all rats die; "rat race".

ratsFromFile :: String -> IO [String]
ratsFromFile path =  -- ??: data filepath
    return ["wardah.21", "wardah.23"]

data Callable = Callable { method :: String, url :: String }
    deriving Show

-- Everything a system-under-test maintainer could ask for when reproducing our
-- counter-example.
--
-- Callable, DNS configs (??: /etc/resolv.conf), Execution time,
data Lead = Lead { c :: Callable, verbose :: Bool }

instance Show Lead where
    show (Lead (Callable m u) v) =
        -- ??: verbose prints fields other than Callable
        show m ++ "\n" ++ show u ++ "\n"


-- GOAL: parallel users (instead of async top-level semantics)
go :: String -> MVar Lead -> IO ()
go rat var = handle handler $ forever $ do
    putStrLn "Rat is walking the script AST..."
    threadDelay 500000

    let l = Lead
            { c = Callable
                  { method = "GET"
                  , url = "https://httpbin.org/get"
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
    var <- newEmptyMVar

    rats <- ratsFromFile ""

    tids <- forM rats $
        \rat -> forkIO (go rat var)

    firstLead <- takeMVar var
    putStrLn $ show firstLead
    forM_ tids killThread
