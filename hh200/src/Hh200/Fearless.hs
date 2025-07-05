module Hh200.Fearless where

-- GOAL: conveniently present first counter-example

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Control.Monad.Reader
-- import qualified Hh200.Types as Hh (Lead)
import qualified Hh200.Types as Hh
import qualified Hh200.Scanner as Hh

data Ckallable = Ckallable { mkethod :: String, ukrl :: String }
    deriving Show

-- Everything a system-under-test maintainer could ask for when reproducing our
-- counter-example.
--
-- Callable, DNS configs (??: /etc/resolv.conf), Execution time,

-- data Lead = Lead { c :: Ckallable, verbose :: Bool }
--
-- instance Show Lead where
--     show (Lead (Ckallable m u) v) =
--         -- ??: verbose prints all fields
--         show m ++ "\n" ++ show u ++ "\n"


-- GOAL: parallel users (instead of async top-level semantics)
-- examples/download.hhs downloads the same file twice
go :: String -> MVar Hh.Lead -> IO ()
go rat var = handle handler $ forever $ do
    putStrLn "Rat is walking the script AST..."
    -- threadDelay 500000
    threadDelay 100000

    putMVar var Hh.defaultLead
    -- return ()

    where

    handler :: AsyncException -> IO ()
    handler ThreadKilled = do
        putStrLn "Rat received ThreadKilled and is cleaning up."
    handler e = throwIO e

ratsFromFile :: String -> IO [String]
ratsFromFile path =  -- ??: data filepath
    return ["wardah.21", "wardah.23"]

ratRace_ :: IO ()
ratRace_ = do
    var <- newEmptyMVar

    rats <- ratsFromFile ""

    tids <- forM rats $
        \rat -> forkIO (go rat var)

    -- firstLead <- takeMVar var
    -- putStrLn $ show firstLead
    forM_ tids killThread

-- runHttpM :: HttpM a -> IO a

ratRace :: IO ()
ratRace = do
    var <- newEmptyMVar

    rats <- ratsFromFile ""

    tids <- forM rats $
        \rat -> forkIO (go rat var)

    putStrLn "here"
    forM_ tids killThread

raceToLead :: Hh.Mini -> IO ()
raceToLead ast = do
    var <- newEmptyMVar

    rats <- ratsFromFile ""

    tids <- forM rats $
        -- \rat -> forkIO (go rat var)
        \rat -> forkIO (Hh200.Fearless.go rat var)

    forM_ tids killThread

    let stacked = Hh.hhsStack ast
    Hh.runHttpM stacked
    threadDelay 100000
