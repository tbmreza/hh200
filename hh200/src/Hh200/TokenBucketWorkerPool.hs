{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- poison pill stops a worker gracefully
-- token bucket specifies capacity and refill rate

module Hh200.TokenBucketWorkerPool
  ( RateLimiter
  , RateLimiterConfig(..)
  , initRateLimiter
  , destroyRateLimiter
  , withRateLimiter
  , waitAndConsumeToken
  , WorkerMode(..)
  , WorkerConfig(..)
  , worker, courier
  , workOptimize, dummyDuo
  , RunState(..)
  ) where

import Debug.Trace
-- import           Control.Concurrent.STM (TVar, STM, TQueue, atomically, readTQueue, newTVar, readTVar, writeTVar, check, modifyTVar')
-- import           Control.Monad.Reader
-- import           Text.Printf (printf)
-- import qualified Hh200.Http as Http
-- import qualified Hh200.Scanner as Scanner

import qualified Data.HashMap.Strict as HM
import           Control.Concurrent
import           Control.Concurrent.Async (async, cancel, Async)
import           Control.Concurrent.STM (TVar, STM, atomically, newTVar, readTVar, writeTVar, check, modifyTVar')
import           Control.Exception (bracket, finally)
import           Control.Monad (forever)

import           Hh200.Types
import           Hh200.Execution (runScriptM, runScriptWith, CourierCtx(..))

-- In all modes, a worker stops on timer or sigint.
-- testSimple:  timer: y  rate-limit: no  feature: Script can be optimized to [Script], fork that number of workers.
-- testRps:     timer: y  rate-limit: y   feature: rampup-able pool of virtual users. each vu takes nap at the end of Script.
-- testShotgun: timer: y  rate-limit: no  feature: of N hits in the same instant, report how many failed

data WorkerMode
  = OneShot            -- testSimple / testShotgun: run Script once, exit
  | LoopWithNap Int    -- testRps: loop Script forever, threadDelay n μs between iterations
    deriving (Show, Eq)

data WorkerConfig = WorkerConfig
  { wcMode        :: WorkerMode
  , wcRateLimiter :: Maybe RateLimiter
  , wcWorkerId    :: Int
  }

-- [CallItem] deps analysis.
workOptimize :: Script -> [Script]
workOptimize s = [s, s]  -- ??:

dummyDuo :: Script -> [Script]
-- dummyDuo s = [s, s]
dummyDuo s = [s]

data RunState = Running | Paused | Stopped
    deriving (Eq)

-- acquireToken :: RateLimiter -> RunState -> STM ()
-- fire :: AppEnv -> RateLimiter -> MVar () -> IO ()
-- fire = loop

-- acquireToken :: TVar RunState -> RateLimite -> STM ()
-- acquireToken s Unlimited = do
--   st <- readTVar s
--   case st of
--     Paused  -> retry
--     Stopped -> return ()
--     Running -> return ()
-- acquireToken s (TokenBucket { tbTokens = tokens }) = do
--   st <- readTVar s
--   case st of
--     Paused  -> retry
--     Stopped -> return ()
--     Running -> do
--       n <- readTVar tokens
--       if n > 0
--         then writeTVar tokens (n - 1)
--         else retry

-- worker :: WorkerConfig -> Script -> TVar RunState -> MVar () -> IO ()
worker :: WorkerConfig -> Script -> TVar Bool -> MVar () -> IO ()
worker    cfg             script    shutdown     done =
    loop `finally` putMVar done ()
    where
    loop = do
        stop <- atomically $ readTVar shutdown
        if stop then
            pure ()
        else do
            -- Rate-limit if configured.
            case wcRateLimiter cfg of
                Just rl -> waitAndConsumeToken rl
                Nothing -> pure ()

            -- acquireToken
            trace ("worker:runScriptM........") $ runScriptM script newEnv

            -- OneShot: exit after one run. LoopWithNap: nap then loop.
            case wcMode cfg of
                OneShot       -> pure ()
                LoopWithNap n -> threadDelay n >> loop

-- Terminates after first iteration on duration=0
-- goal: each vu sends identifiable Origin header: scriptCtx (across CallItems. atm basically Env), courierCtx (throughout script repetition in courier's life. "under this ctx, Origin header is the same")
-- the Env-ScriptCtx analogy that may or may not extend to CourierCtx is the fact that Env grows/folds/mutates throughout its life

-- review this train of thoughts on hammerhead
--
-- 1. buildRequest does set headers to do its function
-- 2. currently buildRequest :: Env -> CallItem -> Http.Request
-- 3. Script comprises [CallItem], all sharing the same courier Origin header
--
-- hypothesis: we should make the monad stack ready made in terms of courier hand off

-- courier :: CourierCtx -> Script -> (TVar RunState, Int) -> MVar () -> IO ()
courier :: Script -> (TVar RunState, Int) -> MVar () -> IO ()
courier    script    (cue, dur)              done =
    loop `finally` putMVar done ()
    where

    loop = do
        stop <- atomically $ readTVar cue
        case stop of
            Stopped -> pure ()
            _ -> do
                let courierCtx = CourierCtx { courierName = "hh200a" }
                trace ("courier:runScriptM........") $ (runScriptWith courierCtx) script newEnv
                -- trace ("courier:runScriptM........") $ runScriptM script newEnv
                threadDelay 1000
                case dur of
                    0 -> pure ()
                    _ -> loop

-- | Configuration for the Rate Limiter
data RateLimiterConfig = RateLimiterConfig
  { bucketCapacity :: Int
  , refillRate     :: Int -- ^ Tokens per second
  } deriving (Show, Eq)

data RateLimite =
    Unlimited  -- network monitoring is probably more interesting here

-- | Token Bucket Rate Limiter
data RateLimiter = RateLimiter
  { rlTokens      :: TVar Int
  , rlConfig      :: RateLimiterConfig
  , rlConsumed    :: TVar Integer -- ^ Total tokens consumed for monitoring
  , rlRefillAsync :: Async ()
  }

-- | Statistics for monitoring
data RateLimiterStats = RateLimiterStats
  { currentTokens :: Int
  , totalConsumed :: Integer
  , capacity      :: Int
  } deriving (Show, Eq)

-- | Initialize a new rate limiter and start the refill thread.
-- Refills are spread across sub-second intervals for smoother rate limiting.
-- Use 'destroyRateLimiter' or 'withRateLimiter' to ensure the refill thread
-- is cancelled when done.
initRateLimiter :: RateLimiterConfig -> IO RateLimiter
initRateLimiter rlc = do
    tokens <- atomically $ newTVar (bucketCapacity rlc)
    consumed <- atomically $ newTVar 0
    let (intervalUs, tokensPerTick)
          | refillRate rlc >= 10 = (100000, refillRate rlc `div` 10)
          | refillRate rlc > 0   = (1000000 `div` refillRate rlc, 1)
          | otherwise               = (1000000, 0) -- rate=0: no refill
    refillAsync <- async $ forever $ do
        threadDelay intervalUs
        atomically $ do
            current <- readTVar tokens
            let newValue = min (bucketCapacity rlc) (current + tokensPerTick)
            writeTVar tokens newValue
    pure $ RateLimiter tokens rlc consumed refillAsync

-- | Cancel the refill thread. Safe to call multiple times.
destroyRateLimiter :: RateLimiter -> IO ()
destroyRateLimiter = cancel . rlRefillAsync

-- | Bracket-style rate limiter lifecycle.
withRateLimiter :: RateLimiterConfig -> (RateLimiter -> IO a) -> IO a
withRateLimiter rlc = bracket (initRateLimiter rlc) destroyRateLimiter

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


