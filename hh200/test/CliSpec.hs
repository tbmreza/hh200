module CliSpec where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Golden
import System.IO.Silently (capture_)
import Options.Applicative
import qualified Data.ByteString.Lazy.Char8 as L8

import Hh200.Cli

spec :: TestTree
spec = testGroup "CLI"
  [
    testCase "Args parsing: --version" $ do
      let res = execParserPure defaultPrefs optsInfo ["--version"]
      case res of
        Success args -> assertEqual "Args" (expectedArgs { version = True }) args
        _ -> assertFailure "Failed to parse --version"

  , testCase "Args parsing: source file" $ do
      let res = execParserPure defaultPrefs optsInfo ["foo.hhs"]
      case res of
        Success args -> assertEqual "Args" (expectedArgs { source = Just "foo.hhs" }) args
        _ -> assertFailure "Failed to parse source file"

  , goldenVsString "Help output" "test/golden/help.txt" $ do
      let res = execParserPure defaultPrefs optsInfo ["--help"]
      case res of
        Failure failure -> do
            let (msg, _) = renderFailure failure "hh200"
            -- renderFailure usually produces the help text without a trailing newline,
            -- but the golden file was created with echo which adds one.
            pure $ L8.pack (msg ++ "\n")
        _ -> assertFailure "Expected failure for help"

  , goldenVsString "Version output" "test/golden/version.txt" $ do
      output <- capture_ $ go (expectedArgs { version = True })
      -- capture_ captures stdout. go prints with putStrLn, so it has a newline.
      pure $ L8.pack output
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
    }
