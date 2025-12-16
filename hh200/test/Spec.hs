{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad.Trans.Maybe
import qualified Network.HTTP.Client as Prim
import qualified Network.HTTP.Client.TLS as Prim (tlsManagerSettings)

import Hh200.Types as Hh
import Hh200.Scanner as Hh
import Hh200.Execution as Hh

import Hh200.Cli

main :: IO ()
main = defaultMain $ testGroup "HUnit"
    -- [ test3_present ]

  [ testLR
  , testLR_mustache
  , testLR_post
  , testLR_invalid
  , testLR_empty
  , testLR_config
  , testLR_tlsInference
  , testBel
  , test1
  , test3_present
  ]


testBel :: TestTree
testBel = testCase "BEL callsite" $ do
    mgr <-        Prim.newManager Prim.tlsManagerSettings
    instant <-    Prim.parseRequest "http://localhost"
    mtRespBody <- Prim.httpLbs instant mgr -- :: Prim.Response L8.ByteString

    ok <-  Hh.assertsAreOk mtHM mtRespBody (rsFrom ["true", "true", "true"])
    neg <- Hh.assertsAreOk mtHM mtRespBody (rsFrom ["true", "false"])

    case (ok, neg) of
        (True, False) -> pure ()
        otherCases -> assertFailure (show otherCases)

    where
    rsFrom :: [String] -> Maybe Hh.ResponseSpec
    rsFrom rsLines = Just $ Hh.ResponseSpec
      { Hh.statuses = []
      , Hh.output = []
      , Hh.captures = Hh.RhsDict mtHM
      , Hh.asserts = rsLines
      }

testLR :: TestTree
testLR = testCase "lexer and parser" $ do
    let tokens = Hh.alexScanTokens "POST http://localhost:9999/echo.php\nAuthorization: Bearer \nTest: $.data.id\nUser-Agent: \"lite\""

    case Hh.parse tokens of
        Hh.ParseOk _ -> do
            pure ()
        Hh.ParseFailed _ -> do
            assertFailure $ show tokens

testLR_mustache :: TestTree
testLR_mustache = testCase "lexer and parser for valid mustached" $ do
    let input = "POST http://httpbin.org/post&unused={{100}}"
        tokens = Hh.alexScanTokens input

    case Hh.parse tokens of
        Hh.ParseOk _ -> pure ()
        Hh.ParseFailed _ -> assertFailure $ "Failed to parse: " ++ show tokens

testLR_post :: TestTree
testLR_post = testCase "lexer and parser for POST" $ do
    let input = "POST http://httpbin.org/post\nContent-Type: application/json\n\n{ \"foo\": \"bar\", \"baz\": 123 }"
        tokens = Hh.alexScanTokens input

    case Hh.parse tokens of
        Hh.ParseOk _ -> pure ()
        Hh.ParseFailed _ -> assertFailure $ "Failed to parse: " ++ show tokens

testLR_invalid :: TestTree
testLR_invalid = testCase "lexer and parser for invalid input" $ do
    let input = "INVALID http://httpbin.org/post"
        tokens = Hh.alexScanTokens input

    case Hh.parse tokens of
        Hh.ParseOk _ -> assertFailure "Should have failed to parse"
        Hh.ParseFailed _ -> pure ()

testLR_empty :: TestTree
testLR_empty = testCase "lexer and parser for empty input" $ do
    let input = ""
        tokens = Hh.alexScanTokens input

    case Hh.parse tokens of
        Hh.ParseOk _ -> assertFailure "Should have failed to parse"
        Hh.ParseFailed _ -> pure ()

testLR_config :: TestTree
testLR_config = testCase "lexer and parser for config" $ do
    let input = "[Configs]\nuse-tls: false\n\nGET http://httpbin.org/get"
        tokens = Hh.alexScanTokens input

    case Hh.parse tokens of
        Hh.ParseOk s -> do
            case Hh.useTls (Hh.config s) of
                Just False -> pure ()
                _ -> assertFailure "Should have parsed use-tls: false"
        Hh.ParseFailed _ -> assertFailure $ "Failed to parse: " ++ show tokens

testLR_tlsInference :: TestTree
testLR_tlsInference = testCase "tls inference from url scheme" $ do
    let inputHttps = Hh.Snippet "GET https://httpbin.org/get"

    (Just sHttps) <- runMaybeT $ Hh.analyze inputHttps
    case Hh.effectiveTls sHttps of
        True -> pure ()
        False -> assertFailure "Should have inferred TLS for https"

    let inputHttp = Hh.Snippet "GET http://httpbin.org/get"

    (Just sHttp) <- runMaybeT $ Hh.analyze inputHttp
    case Hh.effectiveTls sHttp of
        False -> pure ()
        True -> assertFailure "Should have inferred no TLS for http"



test1 :: TestTree
test1 = testCase "linter hints" $ do
    let testCli = Args { call = False, source = Just "../examples/get_json.hhs"
                   , version = False
                   , shotgun = 1
                   , debugConfig = False
                   , rps = False
                   }

    case source testCli of
        Just path -> do
            ms <- runMaybeT (Hh.analyze path)

            case ms of
                Just els@(Hh.Script {Hh.callItems = []}) -> assertFailure $ show els  -- ??: linter model accommodates lsp first and foremost
                                                                                      -- https://github.com/microsoft/vscode-extension-samples/tree/main/lsp-sample
                _ -> pure ()
        Nothing -> assertFailure "Source path is Nothing"


-- test2 :: TestTree
-- test2 = testCase "reality" $ do

-- test3_connect :: TestTree
-- test3_connect = testCase "presentation: graph connect" $ do

-- test3_plot :: TestTree
-- test3_plot = testCase "presentation: graph plot" $ do

test3_present :: TestTree
test3_present = testCase "presentation: cli present" $ do
    let ciHello =
            Hh.CallItem { Hh.ciDeps = []
                        , Hh.ciName = "default"
                        , Hh.ciRequestSpec = Hh.RequestSpec { Hh.verb = Hh.expectUpper "GET"
                                                            , Hh.url = "http://httpbin.org/anything"
                                                            , Hh.headers = Hh.RhsDict mtHM
                                                            , Hh.payload = ""
                                                            , Hh.opts = []
                                                            }
                        , Hh.ciResponseSpec = Just Hh.ResponseSpec
                            { Hh.statuses = [Hh.status200]
                            , Hh.output = []
                            , Hh.captures = Hh.RhsDict mtHM
                            , Hh.asserts = []
                            }
                        }

    expectedHello <- readFile "../examples/hello.hhs"
    assertEqual "hello.hhs" expectedHello (Hh.present ciHello)
