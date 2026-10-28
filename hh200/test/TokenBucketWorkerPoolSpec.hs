module TokenBucketWorkerPoolSpec (spec) where

import           Control.Concurrent
import           Control.Concurrent.STM (atomically, newTVarIO, readTVar, writeTVar)
import           System.Timeout (timeout)
import           Test.Tasty
import           Test.Tasty.HUnit

import           Hh200.TokenBucketWorkerPool
import           Hh200.Types (mkScript)

spec :: TestTree
spec = testGroup "Token bucket worker pool"
  [ testRateLimiterStartsAtCapacity
  , testRateLimiterBlocksWhenEmpty
  , testRateLimiterRefills
  , testRateLimiterCapsTokens
  , testCourierOneShotCompletes
  , testCourierWaitsWhilePaused
  , testCourierStopsBeforeRunning
  , testCourierLoopStopsOnSignal
  ]

testRateLimiterStartsAtCapacity :: TestTree
testRateLimiterStartsAtCapacity = testCase "rate limiter starts full and records consumption" $
  withRateLimiter (RateLimiterConfig 2 0) $ \rl -> do
    waitAndConsumeToken rl
    stats <- atomically (getStats rl)
    assertEqual "one token remains" 1 (currentTokens stats)
    assertEqual "one token consumed" 1 (totalConsumed stats)
    assertEqual "capacity is retained" 2 (capacity stats)

testRateLimiterBlocksWhenEmpty :: TestTree
testRateLimiterBlocksWhenEmpty =
  testCase "rate limiter blocks after capacity is consumed" $
    withRateLimiter (RateLimiterConfig 1 0) $ \rl -> do
      waitAndConsumeToken rl
      result <- timeout 50000 (waitAndConsumeToken rl)
      assertEqual "second consume remains blocked" Nothing result

testRateLimiterRefills :: TestTree
testRateLimiterRefills =
  testCase "rate limiter refills consumed tokens" $
    withRateLimiter (RateLimiterConfig 1 100) $ \rl -> do
      waitAndConsumeToken rl
      result <- timeout 250000 (waitAndConsumeToken rl)
      assertEqual "consume succeeds after refill" (Just ()) result

testRateLimiterCapsTokens :: TestTree
testRateLimiterCapsTokens =
  testCase "rate limiter never exceeds bucket capacity" $
    withRateLimiter (RateLimiterConfig 2 100) $ \rl -> do
      threadDelay 50000
      stats <- atomically (getStats rl)
      assertEqual "tokens stay capped" 2 (currentTokens stats)

testCourierOneShotCompletes :: TestTree
testCourierOneShotCompletes = testCase "courier runs one iteration when duration is zero" $ do
    state <- newTVarIO Running
    done <- newEmptyMVar
    _ <- forkIO (courier (mkScript []) (state, 0) done)
    result <- timeout 1000000 (takeMVar done)
    assertEqual "one-shot courier succeeds" (Just False) result

testCourierWaitsWhilePaused :: TestTree
testCourierWaitsWhilePaused = testCase "courier waits while paused and resumes" $ do
    state <- newTVarIO Paused
    done <- newEmptyMVar
    _ <- forkIO (courier (mkScript []) (state, 0) done)
    threadDelay 50000
    assertBool "courier remains paused" =<< isEmptyMVar done
    atomically (writeTVar state Running)
    result <- timeout 1000000 (takeMVar done)
    assertEqual "courier completes after resume" (Just False) result

testCourierStopsBeforeRunning :: TestTree
testCourierStopsBeforeRunning = testCase "courier does not run after stop" $ do
    state <- newTVarIO Stopped
    done <- newEmptyMVar
    _ <- forkIO (courier (mkScript []) (state, 0) done)
    result <- timeout 1000000 (takeMVar done)
    assertEqual "stopped courier reports no failure" (Just False) result

testCourierLoopStopsOnSignal :: TestTree
testCourierLoopStopsOnSignal = testCase "looping courier exits when stopped" $ do
    state <- newTVarIO Running
    done <- newEmptyMVar
    _ <- forkIO (courier (mkScript []) (state, 1) done)
    threadDelay 30000
    atomically (writeTVar state Stopped)
    result <- timeout 1000000 (takeMVar done)
    assertEqual "loop exits cleanly" (Just False) result
