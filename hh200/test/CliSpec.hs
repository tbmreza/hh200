module CliSpec where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Options.Applicative
import           Control.Concurrent.MVar (MVar)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as Text

import           Hh200.Types
import qualified Hh200.Cli as Cli (go, go')
import           Hh200.Cli

spec :: MVar () -> TestTree
spec _lock = testGroup "CLI"
  [ testGroup "Args parsing"
    [ testCase "source file only" $ do
        let res = execParserPure defaultPrefs optsInfo ["foo.hhs"]
        case res of
            Success args -> assertEqual "Args" (mkArgs { source = Just "foo.hhs" }) args
            _ -> assertFailure "Failed to parse source file"

    , testCase "--version flag" $ do
        let res = execParserPure defaultPrefs optsInfo ["--version", "bar.hhs"]
        case res of
            Success args -> assertEqual "Args" (mkArgs { source = Just "bar.hhs", version = True }) args
            _ -> assertFailure "Failed to parse --version with source"

    , testCase "--debug-config flag" $ do
        let res = execParserPure defaultPrefs optsInfo ["--debug-config", "test.hhs"]
        case res of
            Success args -> assertEqual "Args" (mkArgs { source = Just "test.hhs", debugConfig = True }) args
            _ -> assertFailure "Failed to parse --debug-config"

    , testCase "--call flag" $ do
        let res = execParserPure defaultPrefs optsInfo ["--call", "GET http://example.com"]
        case res of
            Success args -> assertEqual "Args" (mkArgs { call = True, source = Just "GET http://example.com" }) args
            _ -> assertFailure "Failed to parse --call"

    , testCase "--rps flag" $ do
        let res = execParserPure defaultPrefs optsInfo ["--rps", "flow.hhs"]
        case res of
            Success args -> assertEqual "Args" (mkArgs { source = Just "flow.hhs", rps = True }) args
            _ -> assertFailure "Failed to parse --rps"

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

    , testCase "--lsp with port number" $ do
        let res = execParserPure defaultPrefs optsInfo ["--lsp=3000"]
        case res of
            Success args -> assertEqual "Args" (mkArgs { lsp = Just 3000 }) args
            _ -> assertFailure "Failed to parse --lsp=3000"

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
    ]

  , testGroup "Default values"
    [ testCase "all defaults" $ do
        let res = execParserPure defaultPrefs optsInfo ["flow.hhs"]
        case res of
            Success args -> do
                assertEqual "shotgun defaults to 1" 1 (shotgun args)
                assertBool "version defaults to False" (not (version args))
                assertBool "debugConfig defaults to False" (not (debugConfig args))
                assertBool "call defaults to False" (not (call args))
                assertBool "rps defaults to False" (not (rps args))
                assertEqual "lsp defaults to Nothing" Nothing (lsp args)
                assertBool "lspStdio defaults to False" (not (lspStdio args))
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
    ]

  , testGroup "Script execution"
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
  ]
