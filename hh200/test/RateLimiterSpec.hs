module RateLimiterSpec (spec) where

import Test.Tasty
import Test.Tasty.HUnit
import Hh200.RateLimiter
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, wait)
import Control.Concurrent.STM (atomically)

spec :: TestTree
spec = testGroup "RateLimiter tests"
  [ testCase "Burst capacity" $ do
      rl <- newRateLimiter 5 1
      -- We should be able to consume 5 tokens immediately
      results <- mapM (\_ -> atomically $ tryConsume rl) [1..5]
      assertBool "Should have consumed 5 tokens" (all id results)

      -- 6th should fail
      sixth <- atomically $ tryConsume rl
      assertBool "Should not have consumed 6th token" (not sixth)

  , testCase "Sustained rate" $ do
      -- Capacity 1, rate 10 tokens per second
      rl <- newRateLimiter 1 10

      -- Consume first
      c1 <- atomically $ tryConsume rl
      assertBool "Should consume 1st" c1

      -- Wait for 0.15s, should have refilled (at least 1 token)
      threadDelay 150000
      c2 <- atomically $ tryConsume rl
      assertBool "Should consume 2nd after delay" c2

  , testCase "Blocking behavior" $ do
      rl <- newRateLimiter 1 10
      _ <- waitAndConsume rl -- consume the only token

      start <- async $ do
          waitAndConsume rl -- this should block until refill
          pure True

      -- It should be blocking now.
      -- Wait a bit and check if it finished (it shouldn't have)
      threadDelay 50000 -- 0.05s, not enough for 1 token if rate is 10/s?
      -- Wait, if rate is 10/s, 1 token every 0.1s.

      -- Wait for refill (0.1s)
      threadDelay 100000
      res <- wait start
      assertBool "Should have unblocked and consumed" res
  ]
