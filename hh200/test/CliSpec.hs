module CliSpec where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Golden
import           System.IO.Silently (capture_)
import           Options.Applicative
import qualified Data.ByteString.Lazy.Char8 as L8
import           Control.Concurrent.MVar (MVar)

import Hh200.Cli
import qualified Hh200.Cli as Cli (go)

spec :: MVar () -> TestTree
spec _lock = testGroup "CLI"
  [ testCase "Args parsing: source file" $ do
        let res = execParserPure defaultPrefs optsInfo ["foo.hhs"]
        case res of
            Success args -> assertEqual "Args" (mkArgs { source = Just "foo.hhs" }) args
            _ -> assertFailure "Failed to parse source file"

  , testCase "Script execution: simple" $ do
        let simple = mkArgs { shotgun = 1, call = False, rps = False, source = Just "../examples/alpha.hhs" }
        Cli.go simple
  ]

mkArgs :: Args
mkArgs =
    Args
      { source = Nothing
      , version = False
      , debugConfig = False
      , call = False
      , rps = False
      , shotgun = 1
      , lsp = Nothing
      , lspStdio = False
      }
