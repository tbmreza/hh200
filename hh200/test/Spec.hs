{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Test.Tasty
import Test.Tasty.HUnit

import           Control.Monad.Trans.Maybe
import qualified Network.HTTP.Client as Prim
import qualified Network.HTTP.Client.TLS as Prim (tlsManagerSettings)
import qualified Network.HTTP.Client.Internal as PrimInternal
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Network.HTTP.Types as HttpTypes
import           Control.Concurrent.MVar (newMVar)

import Hh200.Types as Hh
import Hh200.Scanner as Hh
import Hh200.Execution as Hh

import Hh200.Cli

import qualified GoldenCli
import qualified BlindLsp
import qualified GoldenNetw
import qualified ContentTypeSpec

-- Separate module for user-facing features: GoldenCli.spec, BlindLsp.spec, GoldenNetw.spec
-- Naming scheme for the 3 steps: testScanner_ testExecution_ testGraph_
main :: IO ()
main = do
    lock <- newMVar ()
    defaultMain $ testGroup ""
      [ GoldenCli.spec lock
      , BlindLsp.spec
      , GoldenNetw.spec lock
      , ContentTypeSpec.spec
      
      , testScanner_lr
      , testScanner_lrMustache
      , testScanner_lrPost
      , testScanner_lrInvalid
      , testScanner_lrEmpty
      , testScanner_TlsInference
      , testExecution_bel
      , testExecution_validJsonBody
      ]
      -- , testScanner_lrConfig


testExecution_bel :: TestTree
testExecution_bel = testCase "BEL callsite" $ do
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

testExecution_validJsonBody :: TestTree
testExecution_validJsonBody = testCase "validJsonBody structure" $ do
    req <- Prim.parseRequest "GET http://localhost/foo"
    let req' = req { Prim.requestHeaders = [("Accept", "application/json")] }
        respBody = "{\"foo\":\"bar\"}"
        -- Constructing a minimal http-client Response.
        resp = PrimInternal.Response
            { PrimInternal.responseStatus = HttpTypes.status200
            , PrimInternal.responseVersion = HttpTypes.http11
            , PrimInternal.responseHeaders = [("Content-Type", "application/json")]
            , PrimInternal.responseBody = L8.pack respBody
            , PrimInternal.responseCookieJar = Prim.createCookieJar []
            , PrimInternal.responseClose' = PrimInternal.ResponseClose (pure ())
            , PrimInternal.responseOriginalRequest = req'
            , PrimInternal.responseEarlyHints = []
            }

    let val = Hh.validJsonBody req' resp

    case val of
        Aeson.Object obj -> do
            assertBool "Has body" $ KeyMap.member "body" obj
            assertBool "Has headers" $ KeyMap.member "headers" obj
            assertBool "Has status" $ KeyMap.member "status" obj
            assertBool "Has request" $ KeyMap.member "request" obj
            
            case KeyMap.lookup "request" obj of
                Just (Aeson.Object reqObj) -> do
                    assertBool "Request has method" $ KeyMap.member "method" reqObj
                    assertBool "Request has headers" $ KeyMap.member "headers" reqObj
                _ -> assertFailure "Request field is not an object"

        _ -> assertFailure "validJsonBody did not return an Object"

testScanner_lr :: TestTree
testScanner_lr = testCase "lexer and parser" $ do
    let tokens = Hh.alexScanTokens "POST http://localhost:9999/echo.php\nAuthorization: Bearer \nTest: $.data.id\nUser-Agent: \"lite\""

    case Hh.parse tokens of
        Hh.ParseOk _ -> do
            pure ()
        Hh.ParseFailed _ _ -> do
            assertFailure $ show tokens

testScanner_lrMustache :: TestTree
testScanner_lrMustache = testCase "lexer and parser for valid mustached" $ do
    let input = "POST http://httpbin.org/post&unused={{100}}"
        tokens = Hh.alexScanTokens input

    case Hh.parse tokens of
        Hh.ParseOk _ -> pure ()
        Hh.ParseFailed _ _ -> assertFailure $ "Failed to parse: " ++ show tokens

testScanner_lrPost :: TestTree
testScanner_lrPost = testCase "lexer and parser for POST" $ do
    let input = "POST http://httpbin.org/post\nContent-Type: application/json\n\n{ \"foo\": \"bar\", \"baz\": 123 }"
        tokens = Hh.alexScanTokens input

    case Hh.parse tokens of
        Hh.ParseOk _ -> pure ()
        Hh.ParseFailed _ _ -> assertFailure $ "Failed to parse: " ++ show tokens

testScanner_lrInvalid :: TestTree
testScanner_lrInvalid = testCase "lexer and parser for invalid input" $ do
    let input = "INVALID http://httpbin.org/post"
        tokens = Hh.alexScanTokens input

    case Hh.parse tokens of
        Hh.ParseOk _ -> assertFailure "Should have failed to parse"
        Hh.ParseFailed _ _ -> pure ()

testScanner_lrEmpty :: TestTree
testScanner_lrEmpty = testCase "lexer and parser for empty input" $ do
    let input = ""
        tokens = Hh.alexScanTokens input

    case Hh.parse tokens of
        Hh.ParseOk _ -> assertFailure "Should have failed to parse"
        Hh.ParseFailed _ _ -> pure ()

-- testScanner_lrConfig :: TestTree
-- testScanner_lrConfig = testCase "lexer and parser for config" $ do
--     let input = "[Configs]\nuse-tls: false\n\nGET http://httpbin.org/get"
--         tokens = Hh.alexScanTokens input
--
--     case Hh.parse tokens of
--         Hh.ParseOk s -> do
--             case Hh.useTls (Hh.config s) of
--                 Just False -> pure ()
--                 _ -> assertFailure "Should have parsed use-tls: false"
--         Hh.ParseFailed _ _ -> assertFailure $ "Failed to parse: " ++ show tokens

testScanner_TlsInference :: TestTree
testScanner_TlsInference = testCase "tls inference from url scheme" $ do
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
