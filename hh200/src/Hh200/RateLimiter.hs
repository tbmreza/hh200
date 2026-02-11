{-# LANGUAGE RecordWildCards #-}

module Hh200.RateLimiter
    ( RateLimiter
    , newRateLimiter
    , stopRateLimiter
    , waitAndConsume
    , tryConsume
    , setRate
    ) where

import Control.Concurrent.STM (TVar, STM, atomically, newTVar, readTVar, writeTVar, check, newTVarIO)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel)
import Control.Monad (forever)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

-- | A thread-safe Token Bucket Rate Limiter.
data RateLimiter = RateLimiter
    { rlTokens   :: TVar Double
    , rlCapacity :: Double
    , rlRate     :: TVar Double
    , rlThread   :: Async ()
    }

-- | Create a new RateLimiter.
-- @newRateLimiter capacity refillRatePerSec@
newRateLimiter :: Double -> Double -> IO RateLimiter
newRateLimiter capacity refillRatePerSec = do
    tokens <- newTVarIO capacity
    rate <- newTVarIO refillRatePerSec
    thread <- async $ refillLoop tokens capacity rate
    pure $ RateLimiter tokens capacity rate thread

-- | Stop the background refill thread.
stopRateLimiter :: RateLimiter -> IO ()
stopRateLimiter rl = cancel (rlThread rl)

-- | The background loop that refills tokens.
refillLoop :: TVar Double -> Double -> TVar Double -> IO ()
refillLoop tokens capacity rateTVar = do
    let intervalMicros = 10000 -- 0.01 second
    let go lastTime = do
            threadDelay intervalMicros
            now <- getCurrentTime
            let diff = realToFrac (diffUTCTime now lastTime)
            atomically $ do
                rate <- readTVar rateTVar
                let tokensToAdd = rate * diff
                current <- readTVar tokens
                writeTVar tokens (min capacity (current + tokensToAdd))
            go now
    getCurrentTime >>= go

-- | Wait until a token is available and consume it. Blocks if no tokens are available.
waitAndConsume :: RateLimiter -> IO ()
waitAndConsume RateLimiter{..} = atomically $ do
    current <- readTVar rlTokens
    check (current >= 1.0)
    writeTVar rlTokens (current - 1.0)

-- | Attempt to consume a token. Returns True if successful, False otherwise.
tryConsume :: RateLimiter -> STM Bool
tryConsume RateLimiter{..} = do
    current <- readTVar rlTokens
    if current >= 1.0
        then do
            writeTVar rlTokens (current - 1.0)
            pure True
        else pure False

-- | Dynamically update the refill rate.
setRate :: RateLimiter -> Double -> STM ()
setRate rl newRate = writeTVar (rlRate rl) newRate
