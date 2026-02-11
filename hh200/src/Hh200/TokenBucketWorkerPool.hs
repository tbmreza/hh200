{-# LANGUAGE ScopedTypeVariables #-}

module Hh200.TokenBucketWorkerPool (main) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, waitAnyCancel, Async)
import Control.Concurrent.STM (atomically, newTQueueIO, writeTQueue, readTQueue, TQueue)
import Control.Monad (forever)
import Data.Time.Clock (getCurrentTime)
import System.Random (randomRIO)
import Text.Printf (printf)
import Hh200.RateLimiter

-- | Configuration
numWorkers :: Int
numWorkers = 3

bucketCapacity :: Double
bucketCapacity = 10

-- | Refill rate: 5 tokens per second
tokensPerSecond :: Double
tokensPerSecond = 5

numVirtualUsers :: Int
numVirtualUsers = 5

-- | A simple job representation
newtype Job = Job { jobId :: Int }
    deriving (Show)

-- | Worker function: Picks jobs from queue, waits for token, processes job
worker :: Int -> TQueue Job -> RateLimiter -> IO ()
worker wId queue rl = forever $ do
    -- 1. Get job (blocks if queue empty)
    job     :: Job <- atomically $ readTQueue queue

    -- 2. Rate Limit (blocks if no tokens)
    waitAndConsume rl

    -- 3. Process
    now <- getCurrentTime
    printf "[Worker %d] Processing %s at %s\n" wId (show job) (show now)

    -- Simulate processing time
    simDelay <- randomRIO (200000, 500000) -- 0.2s to 0.5s
    threadDelay simDelay

-- | Virtual User: Generates jobs randomly
virtualUser :: Int -> TQueue Job -> IO ()
virtualUser uId queue = forever $ do
    -- Simulate "thinking" time
    thinkTime <- randomRIO (1000000, 2000000) -- 1.0s to 2.0s
    threadDelay thinkTime

    -- Create and submit job
    jobIdVal <- randomRIO (1000, 9999)
    let job = Job jobIdVal
    atomically $ writeTQueue queue job
    printf "[User %d] Submitted %s\n" uId (show job)

main :: IO ()
main = do
    putStrLn "Starting Token Bucket Worker Pool Demo..."

    -- Setup shared resources
    jobQueue <- newTQueueIO
    rl <- newRateLimiter bucketCapacity tokensPerSecond

    -- Start Workers
    putStrLn $ "Spawning " ++ show numWorkers ++ " workers."
    workerAsyncs <- mapM (\i -> async $ worker i jobQueue rl) [1..numWorkers]

    -- Start Virtual Users
    putStrLn $ "Spawning " ++ show numVirtualUsers ++ " virtual users."
    userAsyncs <- mapM (\i -> async $ virtualUser i jobQueue) [1..numVirtualUsers]

    -- Wait for any thread to crash/finish (they are infinite loops, so this waits forever until error)
    putStrLn "System running. Press Ctrl+C to stop."
    _ <- waitAnyCancel (workerAsyncs ++ userAsyncs :: [Async ()])
    pure ()
