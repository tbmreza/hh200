module CliSpec where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Golden
import           System.IO.Silently (capture_)
import           Options.Applicative
import qualified Data.ByteString.Lazy.Char8 as L8
import           Control.Concurrent.MVar (MVar)

import Hh200.Cli

spec :: MVar () -> TestTree
spec _lock = testGroup "CLI"
  [ testCase "Args parsing: source file" $ do
      let res = execParserPure defaultPrefs optsInfo ["foo.hhs"]
      case res of
        Success args -> assertEqual "Args" (expectedArgs { source = Just "foo.hhs" }) args
        _ -> assertFailure "Failed to parse source file"

  , goldenVsString "Help output" "test/golden/help.txt" $ do
      let res = execParserPure defaultPrefs optsInfo ["--help"]
      case res of
        Failure failure -> do
            let (msg, _) = renderFailure failure "hh200"
            pure $ L8.pack (msg ++ "\n")
        _ -> assertFailure "Expected failure for help"
  ]

expectedArgs :: Args
expectedArgs =
  Args
    { source = Nothing
    , version = False
    , debugConfig = False
    , call = False
    , rps = False
    , shotgun = 1
    , lsp = Nothing
    }
