{-# LANGUAGE ScopedTypeVariables #-}

module Hh200.Fearless (module Hh200.Fearless) where

-- GOAL: parallel users (instead of async top-level semantics)
-- GOAL: conveniently present first counter-example

import Control.Concurrent
-- import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
-- import Control.Monad.Reader
-- import qualified Hh200.Types as Hh (Lead)
import qualified Hh200.Types as Hh
import qualified Hh200.Scanner as Hh
-- import qualified Hh200.SonOfJ as Hh (putValidJson)  -- ??
import qualified Data.ByteString.Lazy.Char8 as L8

import Control.Concurrent
import Control.Exception (SomeException, catch)
import Control.Monad (forM_, when)
import System.Random (randomRIO)


-- Everything a system-under-test maintainer could ask for when reproducing our
-- counter-example.
--


-- ratsFromFile :: String -> IO [String]
-- ratsFromFile _path =
--     return ["wardah.21", "wardah.23"]

-- forkIO :: IO () -> IO ThreadId

-- ratRace_ :: IO ()
-- ratRace_ = do
--     var <- newEmptyMVar
--
--     rats <- ratsFromFile ""
--
--     tids <- forM rats $
--         \rat -> forkIO (go rat var)
--
--     -- firstLead <- takeMVar var
--     -- putStrLn $ show firstLead
--     forM_ tids killThread

-- go :: String -> MVar Hh.Lead -> IO ()
-- go _rat var = handle handler $ forever $ do
--     putStrLn "Rat is walking the script AST..."
--     -- threadDelay 500000
--     threadDelay 100000
--
--     -- putMVar var Hh.basicLead
--     return ()
--
--     where
--
--     handler :: AsyncException -> IO ()
--     handler ThreadKilled = do
--         putStrLn "Rat received ThreadKilled and is cleaning up."
--     handler e = throwIO e

type RatInfo = String
consign :: Hh.HttpM L8.ByteString -> RatInfo -> IO Hh.Lead
consign stacked name = do
    Hh.runHttpM stacked
    delay <- randomRIO (1, 5)  -- random delay between 1–5 seconds
    threadDelay (delay * 1000000)
    let msg = "Thread " ++ name ++ " finished after " ++ show delay ++ "s"
    -- return msg
    return Hh.basicLead


raceToLead :: (Hh.ScriptConfig, Hh.HttpM L8.ByteString) -> IO ()
-- raceToLead :: (Hh.ScriptConfig, Hh.HttpM L8.ByteString) -> IO Hh.Lead
raceToLead (Hh.ScriptConfig { Hh.subjects = rats }, stacked) = do
    leadVar <- newEmptyMVar

    -- ??: handle empty subjects
    let actions = map (consign stacked) rats

    tids <- forM actions $ \ioAction -> forkIO $ do
        putStrLn "??: BEGIN TRANSACTION"
        r <- ioAction  -- may throw "status codes don't match" inside
                       -- or ?? ""

        -- Put a Lead except MVar rejects it.
        putMVar leadVar r `catch` handleWinner
        putStrLn "END TRANSACTION"

    -- Wait for the first thread to complete
    result <- takeMVar leadVar
    putStrLn $ "First result: " ++ show result

    -- Kill all remaining threads
    forM_ tids $ \tid -> killThread tid

    putStrLn "All other threads cancelled."

    return ()

    where
    handleWinner :: SomeException -> IO ()
    handleWinner e = do
        return ()

    -- tids <- forM rats $
    --     -- \rat -> forkIO (trap leadVar)
    --     \rat -> forkIO (putStrLn rat)
    --
    -- threadDelay 500000
    --
    -- putStrLn "yg ini"
    -- forM_ tids killThread
    -- return Hh.Lead { c = CallItem { }}  -- ??: inverse of stacking [CallItem] to HttpM

-- raceToLead :: Hh.CallItem -> IO ()
-- raceToLead ast = do
--     var <- newEmptyMVar
--
--     rats <- ratsFromFile ""
--
--     tids <- forM rats $
--         -- \rat -> forkIO (go rat var)
--         \rat -> forkIO (Hh200.Fearless.go rat var)
--
--     threadDelay 100000
--     forM_ tids killThread
--
--     let stacked = Hh.stackHh [ast]
--     Hh.runHttpM stacked
--     return ()
