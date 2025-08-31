{-# LANGUAGE ScopedTypeVariables #-}

-- module Hh200.Fearless (module Hh200.Fearless) where
module Hh200.Fearless () where

-- GOAL: parallel users (instead of async top-level semantics)
-- GOAL: conveniently present first counter-example

-- import Control.Concurrent
-- import Control.Concurrent.MVar
-- import Control.Exception
-- import Control.Monad
-- import Control.Monad.Reader
-- import qualified Hh200.Types as Hh
-- import qualified Hh200.Scanner as Hh
-- import qualified Data.ByteString.Lazy.Char8 as L8

-- import Control.Concurrent
-- import Control.Exception (SomeException, catch)
-- import Control.Monad (forM_, when)
-- import System.Random (randomRIO)


-- Everything a system-under-test maintainer could ask for when reproducing our
-- counter-example.

-- consignHttpM :: Hh.HttpM L8.ByteString -> Hh.Subject -> IO Hh.Lead
-- consignHttpM stacked name = do
--     Hh.runHttpM stacked
--     delay <- randomRIO (1, 5)  -- random delay between 1–5 seconds
--     threadDelay (delay * 1000000)
--     let msg = "Thread " ++ "name" ++ " finished after " ++ show delay ++ "s"
--     -- return msg

-- raceToLeadHttpM :: (Hh.ScriptConfig, Hh.HttpM L8.ByteString) -> IO ()
-- raceToLeadHttpM (Hh.ScriptConfig { Hh.subjects = rats }, stacked) = do
--     leadVar <- newEmptyMVar
--
--     let actions = map (consignHttpM stacked) rats
--
--     tids <- forM actions $ \ioAction -> forkIO $ do
--         putStrLn "??: BEGIN TRANSACTION"
--         r <- ioAction  -- may throw "status codes don't match" inside
--                        -- or ?? ""
--
--         -- Put a Lead except MVar rejects it.
--         putMVar leadVar r `catch` handleWinner
--         putStrLn "END TRANSACTION"
--
--     -- Wait for the first thread to complete
--     result <- takeMVar leadVar
--     putStrLn $ "First result: " ++ show result
--
--     -- Kill all remaining threads
--     forM_ tids $ \tid -> killThread tid
--
--     putStrLn "All other threads cancelled."
--
--     return ()
--
--     where
--     handleWinner :: SomeException -> IO ()
--     handleWinner e = do
--         return ()
