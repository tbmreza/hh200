{-# LANGUAGE ScopedTypeVariables #-}

module Hh200.TokenBucketWorkerPool
  ( -- * Rate Limiter
    RateLimiter
  , RateLimiterConfig(..)
  , RateLimiterStats(..)
  , newRateLimiter
  , waitAndConsumeToken
  , runRefill
  , getStats

    -- * Worker Pool
  , Job(..)
  , worker
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, waitAnyCancel, Async)
import Control.Concurrent.STM (TVar, STM, TQueue, atomically, newTQueueIO, writeTQueue, readTQueue, newTVar, readTVar, writeTVar, check, modifyTVar')
import Control.Monad (forever)
import Data.Time.Clock (getCurrentTime)
import System.Random (randomRIO)
import Text.Printf (printf)

-- | Configuration for the Rate Limiter
data RateLimiterConfig = RateLimiterConfig
  { bucketCapacity :: Int
  , refillRate     :: Int -- ^ Tokens per second
  } deriving (Show, Eq)

-- | Token Bucket Rate Limiter
data RateLimiter = RateLimiter
  { rlTokens   :: TVar Int
  , rlConfig   :: RateLimiterConfig
  , rlConsumed :: TVar Integer -- ^ Total tokens consumed for monitoring
  }

-- | Statistics for monitoring
data RateLimiterStats = RateLimiterStats
  { currentTokens :: Int
  , totalConsumed :: Integer
  , capacity      :: Int
  } deriving (Show, Eq)

-- | Create a new rate limiter starting with full capacity
newRateLimiter :: RateLimiterConfig -> STM RateLimiter
newRateLimiter config = do
    t <- newTVar (bucketCapacity config)
    c <- newTVar 0
    pure $ RateLimiter t config c

-- | Refill the bucket periodically according to config
runRefill :: RateLimiter -> IO ()
runRefill rl = forever $ do
    threadDelay 1000000 -- 1 second
    atomically $ do
        current <- readTVar (rlTokens rl)
        let newValue = min (bucketCapacity (rlConfig rl)) (current + refillRate (rlConfig rl))
        writeTVar (rlTokens rl) newValue

-- | Consume a token, blocking if unavailable
waitAndConsumeToken :: RateLimiter -> IO ()
waitAndConsumeToken rl = atomically $ do
    current <- readTVar (rlTokens rl)
    check (current > 0)
    writeTVar (rlTokens rl) (current - 1)
    modifyTVar' (rlConsumed rl) (+1)

-- | Get current statistics
getStats :: RateLimiter -> STM RateLimiterStats
getStats rl = do
    tokens <- readTVar (rlTokens rl)
    consumed <- readTVar (rlConsumed rl)
    pure RateLimiterStats
        { currentTokens = tokens
        , totalConsumed = consumed
        , capacity      = bucketCapacity (rlConfig rl)
        }

-- | A generic job representation
data Job = Job
    { jobAction :: IO ()
    , jobId     :: Int
    }

-- | Worker function: Picks jobs from queue, waits for token, processes job
worker :: Int -> TQueue Job -> RateLimiter -> IO ()
worker wId queue rl = forever $ do
    job <- atomically $ readTQueue queue
    waitAndConsumeToken rl
    jobAction job
