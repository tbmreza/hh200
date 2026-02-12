{-# LANGUAGE ScopedTypeVariables #-}

-- ??: exhaustive list of load-profile fields: ramp, max-rps, delay
-- testOutsideWorld    degenerative
-- testShotgun
-- testRps             typical
--
-- let config = TokenBucketConfig { capacity = 1, refillRate = 0 }
-- let config = TokenBucketConfig { capacity = 5, refillRate = 2 }

module Hh200.TokenBucketWorkerPool (main) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, waitAnyCancel, Async)
import Control.Concurrent.STM (TVar, STM, TQueue, atomically, newTQueueIO, writeTQueue, readTQueue, newTVar, readTVar, writeTVar, check)
-- import Control.Monad (forever, mapM_)
import Control.Monad (forever)
import Data.Time.Clock (getCurrentTime)
import System.Random (randomRIO)
import Text.Printf (printf)

-- | Configuration
numWorkers :: Int
numWorkers = 3

bucketCapacity :: Int
bucketCapacity = 10

-- | Refill rate: 5 tokens per second
tokensPerSecond :: Int
tokensPerSecond = 5

numVirtualUsers :: Int
numVirtualUsers = 5

-- | A simple job representation
newtype Job = Job { jobId :: Int }
    deriving (Show)

-- | Token Bucket Rate Limiter
data RateLimiter = RateLimiter
  { rlTokens   :: TVar Int
  , rlCapacity :: Int
  }

-- | Create a new rate limiter starting with full capacity
newRateLimiter :: Int -> STM RateLimiter
newRateLimiter cap = do
    t <- newTVar cap
    pure $ RateLimiter t cap

-- | Refill the bucket periodically
runRefill :: RateLimiter -> Int -> IO ()
runRefill rl ratePerSec = forever $ do
    threadDelay 1000000 -- 1 second
    atomically $ do
        current <- readTVar (rlTokens rl)
        let newValue = min (rlCapacity rl) (current + ratePerSec)
        writeTVar (rlTokens rl) newValue

-- | Consume a token, blocking if unavailable
waitAndConsumeToken :: RateLimiter -> IO ()
waitAndConsumeToken rl = atomically $ do
    current <- readTVar (rlTokens rl)
    check (current > 0)
    writeTVar (rlTokens rl) (current - 1)

-- | Worker function: Picks jobs from queue, waits for token, processes job
worker :: Int -> TQueue Job -> RateLimiter -> IO ()
worker wId queue rl = forever $ do
    -- 1. Get job (blocks if queue empty)
    -- ??: CallItem --> callJob with goal of composing runRWST with virtualUser; hardcode if needed 1 VU and default max rps
    job     :: Job <- atomically $ readTQueue queue

    -- 2. Rate Limit (blocks if no tokens)
    waitAndConsumeToken rl

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
    thinkTime <- randomRIO (1000000, 2000000) -- 0.5s to 2.0s
    threadDelay thinkTime

    -- Create and submit job
    jobIdVal <- randomRIO (1000, 9999)
    let job = Job jobIdVal
    atomically $ writeTQueue queue job
    printf "[User %d] Submitted %s\n" uId (show job)


-- Starting Token Bucket Worker Pool Demo...
-- Spawning 3 workers.
-- Spawning 5 virtual users.
-- System running. Press Ctrl+C to stop.
-- [User 1] Submitted Job {jobId = 9898}
-- [Worker 1] Processing Job {jobId = 9898} at 2026-01-15 08:32:56.866727942 UTC
-- [User 4] Submitted Job {jobId = 5674}
-- [Worker 2] Processing Job {jobId = 5674} at 2026-01-15 08:32:56.895308397 UTC
-- [Worker 3] Processing Job {jobId = 9394} at 2026-01-15 08:32:57.159864059 UTC
-- [User 5] Submitted Job {jobId = 9394}
-- [User 3] Submitted Job {jobId = 8134}
main :: IO ()
main = do
    putStrLn "Starting Token Bucket Worker Pool Demo..."

    -- Setup shared resources
    jobQueue <- newTQueueIO
    rl <- atomically $ newRateLimiter bucketCapacity

    -- Start Refill Thread
    refillAsync <- async $ runRefill rl tokensPerSecond

    -- Start Workers
    putStrLn $ "Spawning " ++ show numWorkers ++ " workers."
    workerAsyncs <- mapM (\i -> async $ worker i jobQueue rl) [1..numWorkers]

    -- Start Virtual Users
    putStrLn $ "Spawning " ++ show numVirtualUsers ++ " virtual users."
    userAsyncs <- mapM (\i -> async $ virtualUser i jobQueue) [1..numVirtualUsers]

    -- -- let c :: [Control.Concurrent.Async.Internal.Async()] = refillAsync : workerAsyncs ++ userAsyncs
    -- let ra :: Async () = refillAsync
    -- let wa :: [Async ()] = workerAsyncs
    -- let ua :: [Async ()] = userAsyncs
    -- let c :: [Async ()] = refillAsync : workerAsyncs ++ userAsyncs
    -- let d :: [Async ()] = (refillAsync : workerAsyncs) ++ userAsyncs
    -- let e :: [Async ()] = refillAsync : (workerAsyncs ++ userAsyncs)

    -- Wait for any thread to crash/finish (they are infinite loops, so this waits forever until error)
    putStrLn "System running. Press Ctrl+C to stop."
    _ <- waitAnyCancel ((refillAsync : workerAsyncs) ++ userAsyncs :: [Async ()])
    pure ()



-- # (auto) Token Bucket Worker Pool
--
-- This Haskell project demonstrates a concurrent system combining a **Token Bucket Rate Limiter** with a **Worker Pool**. It simulates a scenario where multiple producers (Virtual Users) submit jobs to a queue, and multiple consumers (Workers) process them subject to a global rate limit.
--
-- ## How to Run
--
-- Prerequisites: [Haskell Stack](https://docs.haskellstack.org/en/stable/README/)
--
-- ```bash
-- stack run
-- ```
--
-- ## Architecture
--
-- ### 1. Token Bucket Rate Limiter
-- The system enforces a global processing rate limit using the **Token Bucket** algorithm.
-- - **Bucket**: Holds tokens (permissions to do work).
-- - **Refill**: Tokens are added at a constant rate (e.g., 5 tokens/second).
-- - **Capacity**: The bucket has a maximum size (burst limit).
-- - **Acquisition**: Workers must acquire a token before processing a job. If the bucket is empty, they block until a token is available.
--
-- ### 2. Worker Pool
-- A fixed set of worker threads process jobs from a shared FIFO queue.
-- - **Job Queue**: A `TQueue` (STM) holds pending jobs.
-- - **Workers**: Concurrently pull jobs from the queue. Before processing, they coordinate with the Rate Limiter.
--
-- ### 3. Virtual Users
-- **What constitutes a "Virtual User"?**
--
-- In this simulation, a **Virtual User** is an independent thread acting as a producer of work. It mimics an external entity (like a real human user, a client application, or a sensor) that:
-- 1.  **Exists Concurrently**: Multiple users operate simultaneously.
-- 2.  **Generates Load**: Submits jobs to the system's queue.
-- 3.  **Behaves Stochastically**: Instead of a constant stream, it "thinks" (sleeps) for random intervals between submissions, creating variable and unpredictable traffic patterns.
--
-- This abstraction allows us to test how the system behaves under realistic, non-uniform load conditions.
--
-- ## Code Structure
--
-- - `src/Main.hs`: Contains the entire implementation including the `RateLimiter` logic, `worker` loops, and `virtualUser` simulation.
--
