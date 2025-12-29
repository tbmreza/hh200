module GoldenCli where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Golden
import System.IO.Silently (capture_)
import Options.Applicative
import qualified Data.ByteString.Lazy.Char8 as L8
import Control.Concurrent.MVar (MVar, withMVar)
import Data.Char (isSpace)

import Hh200.Cli

spec :: MVar () -> TestTree
spec lock = testGroup "CLI"
  [
    -- testCase "Args parsing: --version" $ do
    --   let res = execParserPure defaultPrefs optsInfo ["--version"]
    --   case res of
    --     Success args -> assertEqual "Args" (expectedArgs { version = True }) args
    --     _ -> assertFailure "Failed to parse --version"

    testCase "Args parsing: source file" $ do
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

  , goldenVsString "Version output (flaky)" "test/golden/version.txt" $ do
      output <- withMVar lock $ \_ -> capture_ $ go (expectedArgs { version = True })
      -- capture_ captures stdout. go prints with putStrLn, so it has a newline.
      -- Strip ANSI codes and trim leading/trailing whitespace.
      pure $ L8.pack $ dropWhile isSpace $ reverse $ dropWhile isSpace $ reverse $ stripAnsi output
  ]

stripAnsi :: String -> String
stripAnsi s = case s of
    [] -> []
    '\ESC':'[':rest -> 
        let (_codes, remainder) = break (== 'm') rest
        in case remainder of
             ('m':xs) -> stripAnsi xs
             _ -> stripAnsi rest -- Malformed, just continue
    x:xs -> x : stripAnsi xs


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
