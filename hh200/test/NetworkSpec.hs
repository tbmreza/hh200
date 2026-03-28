{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module NetworkSpec where

import Test.Tasty
import Test.Tasty.Golden
import System.IO.Silently (capture_)
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Control.Exception as E
import System.Exit (ExitCode(..))
import Control.Concurrent.MVar (MVar, withMVar)

import Hh200.Cli
import CliSpec (expectedArgs)

spec :: MVar () -> TestTree
spec lock = testGroup "Network (requires localhost:9999)"
  [
    goldenVsString "hello.hhs" "test/golden/hello_net.txt" $ do
      let args = expectedArgs { source = Just "examples/hello.hhs" }
      result <- E.try $ withMVar lock $ \_ -> capture_ $ go args
      case result of
         Left (e :: E.SomeException) -> pure $ L8.pack $ "FAILED: " ++ show e
         Right output -> pure $ L8.pack output

  , goldenVsString "draft.hhs" "test/golden/draft_net.txt" $ do
      let args = expectedArgs { source = Just "examples/draft.hhs" }
      result <- E.try $ withMVar lock $ \_ -> capture_ $ go args
      case result of
         Left (e :: E.SomeException) -> pure $ L8.pack $ "FAILED: " ++ show e
         Right output -> pure $ L8.pack output
  ]
