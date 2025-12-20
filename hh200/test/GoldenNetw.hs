{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GoldenNetw where

import Test.Tasty
import Test.Tasty.Golden
import System.IO.Silently (capture_)
import Data.IORef
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Control.Exception as E

import Hh200.Cli
import GoldenCli (expectedArgs)

spec :: TestTree
spec = withResource (newIORef True) (\_ -> pure ()) $ \refIO ->
  testGroup "Network (requires localhost:9999)"
  [
    goldenVsString "hello.hhs" "test/golden/hello_net.txt" $ do
      ref <- refIO
      let args = expectedArgs { source = Just "examples/hello.hhs" }
      result <- E.try $ capture_ $ go args
      case result of
         Left (e :: E.SomeException) -> do
             writeIORef ref False
             pure $ L8.pack $ "FAILED: " ++ show e
         Right output -> do
             pure $ L8.pack output

  , goldenVsString "draft.hhs" "test/golden/draft_net.txt" $ do
      ref <- refIO
      ok <- readIORef ref
      if not ok
         then pure "SKIPPED_DUE_TO_FAILURE"
         else do
             let args = expectedArgs { source = Just "examples/draft.hhs" }
             result <- E.try $ capture_ $ go args
             case result of
                Left (e :: E.SomeException) -> pure $ L8.pack $ "FAILED: " ++ show e
                Right output -> pure $ L8.pack output
  ]

