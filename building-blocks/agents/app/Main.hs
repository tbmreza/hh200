-- {-# LANGUAGE OverloadedStrings #-}


module Main (main) where

-- GOAL: conveniently present first counter-example
-- import Control.Parallel
import Control.Concurrent
import Control.Monad
import Lib

ratsFromFile :: String -> IO [String]
ratsFromFile path =  -- ??: data filepath
    return ["wardah.21", "wardah.23"]

data Callable = Callable { method :: String, url :: String }
    deriving Show

-- Everything a system-under-test maintainer could ask for in reproducing our
-- counter-example.
--
-- Callable, DNS configs (??: /etc/resolv.conf), Execution time,
data Lead = Lead { c :: Callable, verbose :: Bool }

instance Show Lead where
    show (Lead (Callable m u) v) =
        -- ??: verbose prints fields other than Callable
        show m ++ "\n" ++ show u ++ "\n"

go :: String -> IO ()
go rat = do
    let found = False
    case found of
        True -> putStrLn $ rat ++ " found counter-example:\n\n" ++ show 1
        _ -> return ()


main :: IO ()
main = do
    let l = Lead
            { c = Callable
                { method = "GET"
                , url = "https://httpbin.org/get"
                }
            , verbose = False
            }

    rats <- ratsFromFile ""
    -- PICKUP communicate to other rats that they can stop when we've found a counter-example
    forM_ rats $
        \rat -> forkIO (go rat)


    threadDelay 1000000  -- wait to let all threads finish
    return ()
