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
  , VUState(..)
  -- , worker
  ) where

import Debug.Trace

import qualified Data.HashMap.Strict as HM

-- import Control.Concurrent (threadDelay)
import Control.Concurrent
import Control.Concurrent.Async (async, cancel, Async)
-- import Control.Concurrent.STM (TVar, STM, TQueue, atomically, readTQueue, newTVar, readTVar, writeTVar, check, modifyTVar')
import           Control.Concurrent.STM
import Control.Exception (bracket, finally)
import Control.Monad (forever)
import           Control.Monad.Reader
import Text.Printf (printf)
import           Hh200.Types
import qualified Hh200.Http as Http
import           Hh200.Execution
import qualified Hh200.Scanner as Scanner

-- A Job consumes as many tokens as the number of CallItems a Script contains.
type Job = Maybe Script

-- ms :: Maybe Script
-- ms = Scanner.analyze ("/home/tbmreza/gh/hh200/examples/post_json.hhs" :: FilePath)

-- Here be nested Maybe types: emergency globalShutdown and Job's poison pill.

worker :: TVar Bool -> IO ()
worker    shutdownFlag =
    loop where
    loop = do
        shouldStop <- atomically $ do
            readTVar shutdownFlag

        if shouldStop then
            pure () 
        else
        --     else processJob >> worker shutdownFlag
            undefined

type Global = Int
type Result = Int

processJob :: IO ()
processJob = putStrLn "processJob..."
-- processJob :: TQueue Result -> Job -> ReaderT Global IO ()
-- processJob = undefined

-- ??: runs forever
-- worker :: VUState -> TChan Job -> (TVar Int, TVar Bool) -> MVar () -> IO ()
-- worker    vu         jobChan      (bucket, globalShutdown) done =
--     loop where
--     loop = do
--         result <- atomically $
--             -- Path A: check if emergency shutdown has been triggered
--             (trace "sup" $ (do
--                 -- let ms :: Maybe Script = Scanner.analyze ("/home/tbmreza/gh/hh200/examples/post_json.hhs" :: FilePath)
--                 shutdown <- readTVar globalShutdown
--                 -- check shutdown          -- retries if False
--                 pure (Just $ Just mkScript)))
--             `orElse`
--             -- Path B: read a job from the channel as normal
--             -- (trace "inf" $ (Just <$> readTChan jobChan))
--             undefined
--         case result of
--             -- goal: running runRWST from worker
--             Nothing -> do
--                 -- Global emergency shutdown detected
--                 -- putStrLn $ "Worker " ++ show id ++ " EMERGENCY shutdown!"
--                 putStrLn $ " EMERGENCY shutdown!"
--                 putMVar done ()
--
--             Just (Just script :: Job) -> do
--                 let mgr = Http.newManager True
--                 (mci, finalEnv, procLog) <- runScriptM script HM.empty
--                 putStrLn $ " processing: "
--                 loop
--             Just (Nothing :: Job) -> do
--                 putStrLn $ " shutting down (pill)."
--                 putMVar done ()

-- runRWST (runMaybeT course) ctx env
-- runProcM :: Script -> ExecContext -> Env -> IO (Maybe CallItem, Env, Log)
-- conduct ::  Script -> ExecContext -> Env -> IO Lead
processTask :: VUState -> Script -> IO ()
processTask vu script = putStrLn $ "Worker x finished a task"

data VUState = VUState
  { workerId :: Int
  }



-- | Configuration for the Rate Limiter
data RateLimiterConfig = RateLimiterConfig
  { bucketCapacity :: Int
  , refillRate     :: Int -- ^ Tokens per second
  } deriving (Show, Eq)

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
initRateLimiter config = do
    tokens <- atomically $ newTVar (bucketCapacity config)
    consumed <- atomically $ newTVar 0
    let (intervalUs, tokensPerTick)
          | refillRate config >= 10 = (100000, refillRate config `div` 10)
          | refillRate config > 0   = (1000000 `div` refillRate config, 1)
          | otherwise               = (1000000, 0) -- rate=0: no refill
    refillAsync <- async $ forever $ do
        threadDelay intervalUs
        atomically $ do
            current <- readTVar tokens
            let newValue = min (bucketCapacity config) (current + tokensPerTick)
            writeTVar tokens newValue
    pure $ RateLimiter tokens config consumed refillAsync

-- | Cancel the refill thread. Safe to call multiple times.
destroyRateLimiter :: RateLimiter -> IO ()
destroyRateLimiter = cancel . rlRefillAsync

-- | Bracket-style rate limiter lifecycle.
withRateLimiter :: RateLimiterConfig -> (RateLimiter -> IO a) -> IO a
withRateLimiter config = bracket (initRateLimiter config) destroyRateLimiter

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
data Jib = Jib
    { jobAction :: IO ()
    , jobId     :: Int
    }

-- | Worker: pulls jobs from queue, consumes a rate-limiter token, runs the job.
-- Exits cleanly on 'Nothing' (poison pill).
workir :: Int -> TQueue (Maybe Jib) -> RateLimiter -> IO ()
workir wId queue rl = go
  where
  go = do
    mjob <- atomically $ readTQueue queue
    case mjob of
      Nothing  -> printf "Worker %d: shutting down\n" wId
      Just job -> do
        waitAndConsumeToken rl
        printf "Worker %d: job %d\n" wId (jobId job)
        jobAction job
        go
