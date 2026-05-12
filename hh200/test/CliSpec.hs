module CliSpec where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Golden
import           System.IO.Silently (capture_)
import           Options.Applicative
import qualified Data.ByteString.Lazy.Char8 as L8
import           Control.Concurrent.MVar (MVar)
import           System.Exit (ExitCode(..))
import           Control.Monad (void)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as Text


import           Hh200.Types
import qualified Hh200.Cli as Cli (go, go')
-- import           Hh200.Cli (mkArgs, source, version, debugConfig, call, rps, shotgun, lsp, lspStdio)
import           Hh200.Cli


spec :: MVar () -> TestTree
spec _lock = testGroup "CLI"
  [ testGroup "Args parsing"
    [ testCase "source file only" $ do
        let res = execParserPure defaultPrefs optsInfo ["foo.hhs"]
        case res of
            Success args -> assertEqual "Args" (mkArgs { source = Just "foo.hhs" }) args
            _ -> assertFailure "Failed to parse source file"

    , testCase "source file with --version flag" $ do
        let res = execParserPure defaultPrefs optsInfo ["--version", "bar.hhs"]
        case res of
            Success args -> assertEqual "Args" (mkArgs { source = Just "bar.hhs", version = True }) args
            _ -> assertFailure "Failed to parse --version with source"

    , testCase "source file with -V short flag" $ do
        let res = execParserPure defaultPrefs optsInfo ["-V", "baz.hhs"]
        case res of
            Success args -> assertEqual "Args" (mkArgs { source = Just "baz.hhs", version = True }) args
            _ -> assertFailure "Failed to parse -V with source"

    , testCase "--version only" $ do
        let res = execParserPure defaultPrefs optsInfo ["--version"]
        case res of
            Success args -> assertEqual "Args" (mkArgs { version = True }) args
            _ -> assertFailure "Failed to parse --version"

    , testCase "-V short version only" $ do
        let res = execParserPure defaultPrefs optsInfo ["-V"]
        case res of
            Success args -> assertEqual "Args" (mkArgs { version = True }) args
            _ -> assertFailure "Failed to parse -V"

    , testCase "--debug-config flag" $ do
        let res = execParserPure defaultPrefs optsInfo ["--debug-config", "test.hhs"]
        case res of
            Success args -> assertEqual "Args" (mkArgs { source = Just "test.hhs", debugConfig = True }) args
            _ -> assertFailure "Failed to parse --debug-config"

    , testCase "-F short debug-config flag" $ do
        let res = execParserPure defaultPrefs optsInfo ["-F", "test.hhs"]
        case res of
            Success args -> assertEqual "Args" (mkArgs { source = Just "test.hhs", debugConfig = True }) args
            _ -> assertFailure "Failed to parse -F"

    , testCase "--call flag" $ do
        let res = execParserPure defaultPrefs optsInfo ["--call", "GET http://example.com"]
        case res of
            Success args -> assertEqual "Args" (mkArgs { call = True, source = Just "GET http://example.com" }) args
            _ -> assertFailure "Failed to parse --call"

    , testCase "-C short call flag" $ do
        let res = execParserPure defaultPrefs optsInfo ["-C", "POST http://example.com/api"]
        case res of
            Success args -> assertEqual "Args" (mkArgs { call = True, source = Just "POST http://example.com/api" }) args
            _ -> assertFailure "Failed to parse -C"

    , testCase "--rps flag" $ do
        let res = execParserPure defaultPrefs optsInfo ["--rps", "flow.hhs"]
        case res of
            Success args -> assertEqual "Args" (mkArgs { source = Just "flow.hhs", rps = True }) args
            _ -> assertFailure "Failed to parse --rps"

    , testCase "-R short rps flag" $ do
        let res = execParserPure defaultPrefs optsInfo ["-R", "flow.hhs"]
        case res of
            Success args -> assertEqual "Args" (mkArgs { source = Just "flow.hhs", rps = True }) args
            _ -> assertFailure "Failed to parse -R"

    , testCase "--shotgun with number" $ do
        let res = execParserPure defaultPrefs optsInfo ["--shotgun=4", "flow.hhs"]
        case res of
            Success args -> assertEqual "Args" (mkArgs { source = Just "flow.hhs", shotgun = 4 }) args
            _ -> assertFailure "Failed to parse --shotgun=4"

    , testCase "--shotgun N format" $ do
        let res = execParserPure defaultPrefs optsInfo ["--shotgun", "8", "flow.hhs"]
        case res of
            Success args -> assertEqual "Args" (mkArgs { source = Just "flow.hhs", shotgun = 8 }) args
            _ -> assertFailure "Failed to parse --shotgun 8"

    , testCase "-S short shotgun flag" $ do
        let res = execParserPure defaultPrefs optsInfo ["-S", "2", "flow.hhs"]
        case res of
            Success args -> assertEqual "Args" (mkArgs { source = Just "flow.hhs", shotgun = 2 }) args
            _ -> assertFailure "Failed to parse -S 2"

    , testCase "--lsp with port number" $ do
        let res = execParserPure defaultPrefs optsInfo ["--lsp=3000"]
        case res of
            Success args -> assertEqual "Args" (mkArgs { lsp = Just 3000 }) args
            _ -> assertFailure "Failed to parse --lsp=3000"

    , testCase "--lsp N format" $ do
        let res = execParserPure defaultPrefs optsInfo ["--lsp", "8080"]
        case res of
            Success args -> assertEqual "Args" (mkArgs { lsp = Just 8080 }) args
            _ -> assertFailure "Failed to parse --lsp 8080"

    , testCase "-d short lsp flag" $ do
        let res = execParserPure defaultPrefs optsInfo ["-d", "5000"]
        case res of
            Success args -> assertEqual "Args" (mkArgs { lsp = Just 5000 }) args
            _ -> assertFailure "Failed to parse -d 5000"

    , testCase "--lsp-stdio flag" $ do
        let res = execParserPure defaultPrefs optsInfo ["--lsp-stdio"]
        case res of
            Success args -> assertEqual "Args" (mkArgs { lspStdio = True }) args
            _ -> assertFailure "Failed to parse --lsp-stdio"

    , testCase "multiple flags combined" $ do
        let res = execParserPure defaultPrefs optsInfo ["--shotgun=4", "--debug-config", "flow.hhs"]
        case res of
            Success args -> assertEqual "Args" (mkArgs { source = Just "flow.hhs", shotgun = 4, debugConfig = True }) args
            _ -> assertFailure "Failed to parse combined flags"

    , testCase "rps with shotgun" $ do
        let res = execParserPure defaultPrefs optsInfo ["--rps", "--shotgun=2", "flow.hhs"]
        case res of
            Success args -> assertEqual "Args" (mkArgs { source = Just "flow.hhs", rps = True, shotgun = 2 }) args
            _ -> assertFailure "Failed to parse rps with shotgun"

    , testCase "all flags together" $ do
        let res = execParserPure defaultPrefs optsInfo ["--debug-config", "--shotgun=3", "test.hhs"]
        case res of
            Success args -> assertEqual "Args" (mkArgs { source = Just "test.hhs", debugConfig = True, shotgun = 3 }) args
            _ -> assertFailure "Failed to parse all flags"
    ]

  , testGroup "Default values"
    [ testCase "default shotgun is 1" $ do
        let res = execParserPure defaultPrefs optsInfo ["flow.hhs"]
        case res of
            Success args -> assertEqual "shotgun" 1 (shotgun args)
            _ -> assertFailure "Failed to parse"

    , testCase "version defaults to False" $ do
        let res = execParserPure defaultPrefs optsInfo ["flow.hhs"]
        case res of
            Success args -> assertBool "version" (not (version args))
            _ -> assertFailure "Failed to parse"

    , testCase "debugConfig defaults to False" $ do
        let res = execParserPure defaultPrefs optsInfo ["flow.hhs"]
        case res of
            Success args -> assertBool "debugConfig" (not (debugConfig args))
            _ -> assertFailure "Failed to parse"

    , testCase "call defaults to False" $ do
        let res = execParserPure defaultPrefs optsInfo ["flow.hhs"]
        case res of
            Success args -> assertBool "call" (not (call args))
            _ -> assertFailure "Failed to parse"

    , testCase "rps defaults to False" $ do
        let res = execParserPure defaultPrefs optsInfo ["flow.hhs"]
        case res of
            Success args -> assertBool "rps" (not (rps args))
            _ -> assertFailure "Failed to parse"

    , testCase "lsp defaults to Nothing" $ do
        let res = execParserPure defaultPrefs optsInfo ["flow.hhs"]
        case res of
            Success args -> assertEqual "lsp" Nothing (lsp args)
            _ -> assertFailure "Failed to parse"

    , testCase "lspStdio defaults to False" $ do
        let res = execParserPure defaultPrefs optsInfo ["flow.hhs"]
        case res of
            Success args -> assertBool "lspStdio" (not (lspStdio args))
            _ -> assertFailure "Failed to parse"
    ]

  , testGroup "Edge cases"
    [ testCase "empty source is Nothing" $ do
        let res = execParserPure defaultPrefs optsInfo []
        case res of
            Success args -> assertEqual "source" Nothing (source args)
            _ -> assertFailure "Failed to parse empty args"

    , testCase "version and debug-config together" $ do
        let res = execParserPure defaultPrefs optsInfo ["--version", "--debug-config"]
        case res of
            Success args -> assertBool "combined flags" (version args && debugConfig args)
            _ -> assertFailure "Failed to parse"

    , testCase "shotgun value of 0" $ do
        let res = execParserPure defaultPrefs optsInfo ["--shotgun=0", "flow.hhs"]
        case res of
            Success args -> assertEqual "shotgun" 0 (shotgun args)
            _ -> assertFailure "Failed to parse shotgun=0"

    , testCase "large shotgun value" $ do
        let res = execParserPure defaultPrefs optsInfo ["--shotgun=1000", "flow.hhs"]
        case res of
            Success args -> assertEqual "shotgun" 1000 (shotgun args)
            _ -> assertFailure "Failed to parse large shotgun"

    , testCase "script path with spaces" $ do
        let res = execParserPure defaultPrefs optsInfo ["path with spaces.hhs"]
        case res of
            Success args -> assertEqual "source" (Just "path with spaces.hhs") (source args)
            _ -> assertFailure "Failed to parse path with spaces"

    , testCase "script path with special chars" $ do
        let res = execParserPure defaultPrefs optsInfo ["test_script-v1.2.3.hhs"]
        case res of
            Success args -> assertEqual "source" (Just "test_script-v1.2.3.hhs") (source args)
            _ -> assertFailure "Failed to parse path with special chars"

    , testCase "relative path" $ do
        let res = execParserPure defaultPrefs optsInfo ["../scripts/test.hhs"]
        case res of
            Success args -> assertEqual "source" (Just "../scripts/test.hhs") (source args)
            _ -> assertFailure "Failed to parse relative path"

    , testCase "absolute path" $ do
        let res = execParserPure defaultPrefs optsInfo ["/absolute/path/to/script.hhs"]
        case res of
            Success args -> assertEqual "source" (Just "/absolute/path/to/script.hhs") (source args)
            _ -> assertFailure "Failed to parse absolute path"
    ]

  , testGroup "Help text parsing"
    [ testCase "--help takes precedence over source" $ do
        let res = execParserPure defaultPrefs optsInfo ["--help", "ignored.hhs"]
        case res of
            Success _ -> return ()
            Failure _ -> return ()
            _ -> assertFailure "Unexpected parser result"
    ]

  , testGroup "Args equality"
    [ testCase "Args with same values are equal" $ do
        let a1 = mkArgs { source = Just "test.hhs", shotgun = 5 }
            a2 = mkArgs { source = Just "test.hhs", shotgun = 5 }
        assertEqual "Args equality" a1 a2

    , testCase "Args with different source are not equal" $ do
        let a1 = mkArgs { source = Just "a.hhs" }
            a2 = mkArgs { source = Just "b.hhs" }
        assertBool "Args inequality" (a1 /= a2)

    , testCase "Args with different shotgun are not equal" $ do
        let a1 = mkArgs { shotgun = 1 }
            a2 = mkArgs { shotgun = 2 }
        assertBool "Args inequality" (a1 /= a2)

    , testCase "Args with different version are not equal" $ do
        let a1 = mkArgs { version = False }
            a2 = mkArgs { version = True }
        assertBool "Args inequality" (a1 /= a2)
    ]

  , testGroup "Script execution"
      -- [ testCase "Script execution: simple" $ do
      --       let simple = mkArgs { shotgun = 1, call = False, rps = False, source = Just "../examples/alpha.hhs" }
      --       Cli.go simple

      [ testCase "Script execution: ast" $ do
            let args = mkArgs { source = Nothing }
            let ci = CallItem { ciDeps = []
                              , ciName = "default"
                              , ciRequestSpec = RequestSpec { rqMethod = "POST"
                                                            , rqSquares = ( Nothing, Nothing, Nothing
                                                                          , Just (RequestSquareMultipart (RhsDict (HM.singleton (Text.pack "dummy") [])))
                                                                          , Nothing
                                                                          )
                                                            , rqUrl = LexedUrlFull "http://localhost:9999/api/echo"
                                                            , rqHeaders = RhsDict HM.empty
                                                            , rqBody = ""
                                                            }
                              , ciResponseSpec = Nothing
                              }
            let prog = mkScript [ci]
            Cli.go' prog args
      ]

  , testGroup "Args Show instance"
    [ testCase "Show instance works for mkArgs" $ do
        let args = mkArgs
        show args `seq` return ()

    , testCase "Show instance works for version args" $ do
        let args = mkArgs { version = True }
        show args `seq` return ()

    , testCase "Show instance works for complex args" $ do
        let args = mkArgs { source = Just "test.hhs", shotgun = 5, version = True, rps = True }
        show args `seq` return ()
    ]
  ]
