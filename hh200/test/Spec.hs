{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Test.Tasty
import Test.Tasty.HUnit

import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Except (runExceptT)
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
      
      -- , testScanner_lr
      -- , testScanner_lrMustache
      -- , testScanner_lrPost
      -- , testScanner_lrPostMultiline
      -- , testScanner_lrInvalid
      -- , testScanner_lrEmpty
      -- -- , testScanner_TlsInference
      -- , testScanner_lrResponseOrder
      -- , testScanner_lrRequestConfigs
      -- -- , testScanner_analyzeg

      , testExecution_bel
      , testExecution_validJsonBody
      -- , testTypes_presentg
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

-- testScanner_lr :: TestTree
-- testScanner_lr = testCase "lexer and parser" $ do
--     let tokens = Hh.alexScanTokens "POST http://localhost:9999/echo.php\nAuthorization: Bearer \nTest: $.data.id\nUser-Agent: \"lite\""
--
--     res <- runExceptT (Hh.parsek tokens)
--     case res of
--         Right _ -> do
--             pure ()
--         Left _ -> do
--             assertFailure $ show tokens
--
-- testScanner_lrMustache :: TestTree
-- testScanner_lrMustache = testCase "lexer and parser for valid mustached" $ do
--     let input = "POST http://httpbin.org/post&unused={{100}}"
--         tokens = Hh.alexScanTokens input
--
--     res <- runExceptT (Hh.parsek tokens)
--     case res of
--         Right _ -> pure ()
--         Left _ -> assertFailure $ "Failed to parse: " ++ show tokens
--
-- testScanner_lrPost :: TestTree
-- testScanner_lrPost = testCase "lexer and parser for POST" $ do
--     let input = "POST http://httpbin.org/post\nContent-Type: application/json\n\n{ \"foo\": \"bar\", \"baz\": 123 }"
--         tokens = Hh.alexScanTokens input
--
--     res <- runExceptT (Hh.parsek tokens)
--     case res of
--         Right _ -> pure ()
--         Left _ -> assertFailure $ "Failed to parse: " ++ show tokens
--
-- testScanner_lrPostMultiline :: TestTree
-- testScanner_lrPostMultiline = testCase "lexer and parser for POST with multiline JSON" $ do
--     let input = "POST http://httpbin.org/post\nContent-Type: application/json\n\n{\n  \"foo\": \"bar\",\n  \"baz\": 123\n}"
--         tokens = Hh.alexScanTokens input
--
--     res <- runExceptT (Hh.parsek tokens)
--     case res of
--         Right _ -> pure ()
--         Left _ -> assertFailure $ "Failed to parse: " ++ show tokens
--
-- testScanner_lrInvalid :: TestTree
-- testScanner_lrInvalid = testCase "lexer and parser for invalid input" $ do
--     let input = "INVALID http://httpbin.org/post"
--         tokens = Hh.alexScanTokens input
--
--     res <- runExceptT (Hh.parsek tokens)
--     case res of
--         Right _ -> assertFailure "Should have failed to parse"
--         Left _ -> pure ()
--
-- testScanner_lrEmpty :: TestTree
-- testScanner_lrEmpty = testCase "lexer and parser for empty input" $ do
--     let input = ""
--         tokens = Hh.alexScanTokens input
--
--     res <- runExceptT (Hh.parsek tokens)
--     case res of
--         Right _ -> assertFailure "Should have failed to parse"
--         Left _ -> pure ()
--
-- -- testScanner_lrConfig :: TestTree
-- -- testScanner_lrConfig = testCase "lexer and parser for config" $ do
-- --     let input = "[Configs]\nuse-tls: false\n\nGET http://httpbin.org/get"
-- --         tokens = Hh.alexScanTokens input
-- --
-- --     case Hh.parsek tokens of
-- --         Hh.ParseOk s -> do
-- --             case Hh.useTls (Hh.config s) of
-- --                 Just False -> pure ()
-- --                 _ -> assertFailure "Should have parsed use-tls: false"
-- --         Hh.ParseFailed _ _ -> assertFailure $ "Failed to parse: " ++ show tokens
--
-- -- testScanner_TlsInference :: TestTree
-- -- testScanner_TlsInference = testCase "tls inference from url scheme" $ do
-- --     let inputHttps = Hh.Snippet "GET https://httpbin.org/get"
-- --
-- --     (Just sHttps) <- runMaybeT $ Hh.analyze inputHttps
-- --     case Hh.effectiveTls sHttps of
-- --         True -> pure ()
-- --         False -> assertFailure "Should have inferred TLS for https"
-- --
-- --     let inputHttp = Hh.Snippet "GET http://httpbin.org/get"
-- --
-- --     (Just sHttp) <- runMaybeT $ Hh.analyze inputHttp
-- --     case Hh.effectiveTls sHttp of
-- --         False -> pure ()
-- --         True -> assertFailure "Should have inferred no TLS for http"
--
-- -- (auto)
-- testScanner_lrResponseOrder :: TestTree
-- testScanner_lrResponseOrder = testGroup "lexer and parser for response block order"
--     [ testCase "Captures before Asserts" $ do
--         let input = "GET http://localhost\n\nHTTP 200\n[Captures]\nfoo: bar\n\n[Asserts]\n> true\n"
--             tokens = Hh.alexScanTokens input
--         res <- runExceptT (Hh.parsek tokens)
--         case res of
--             Right action -> do
--                 res2 <- runExceptT action
--                 case res2 of
--                     Right s -> do
--                         let ci = head (Hh.callItems s)
--                         case Hh.ciResponseSpec ci of
--                             Just rs -> do
--                                 assertBool "Has captures" $ not (Hh.mtHM == (let Hh.RhsDict hm = Hh.captures rs in hm))
--                                 assertBool "Has asserts" $ not (null (Hh.asserts rs))
--                             Nothing -> assertFailure "Should have response spec"
--                     Left (err, _) -> assertFailure $ "Failed to parse: " ++ err
--             Left (err, _) -> assertFailure $ "Failed to parse: " ++ err
--
--     , testCase "Asserts before Captures" $ do
--         let input = "GET http://localhost\n\nHTTP 200\n[Asserts]\n> true\n\n[Captures]\nfoo: bar\n"
--             tokens = Hh.alexScanTokens input
--         res <- runExceptT (Hh.parsek tokens)
--         case res of
--             Right action -> do
--                 res2 <- runExceptT action
--                 case res2 of
--                     Right s -> do
--                         let ci = head (Hh.callItems s)
--                         case Hh.ciResponseSpec ci of
--                             Just rs -> do
--                                 assertBool "Has captures" $ not (Hh.mtHM == (let Hh.RhsDict hm = Hh.captures rs in hm))
--                                 assertBool "Has asserts" $ not (null (Hh.asserts rs))
--                             Nothing -> assertFailure "Should have response spec"
--                     Left (err, _) -> assertFailure $ "Failed to parse: " ++ err
--             Left (err, _) -> assertFailure $ "Failed to parse: " ++ err
--     ]
--
-- testScanner_lrRequestConfigs :: TestTree
-- testScanner_lrRequestConfigs = testCase "lexer and parser for request configs" $ do
--     let input = "GET http://localhost\nAuthorization: Bearer token\n[Configs]\nretry: 3\n\n{ \"body\": \"here\" }\n"
--         tokens = Hh.alexScanTokens input
--     res <- runExceptT (Hh.parsek tokens)
--     case res of
--         Right action -> do
--             res2 <- runExceptT action
--             case res2 of
--                 Right s -> do
--                     let ci = head (Hh.callItems s)
--                         rs = Hh.ciRequestSpec ci
--                     assertBool "Has configs" $ not (Hh.mtHM == (let Hh.RhsDict hm = Hh.configsg rs in hm))
--                     assertEqual "Payload correct" "{ \"body\": \"here\" }" (Hh.payloadg rs)
--                 Left (err, _) -> assertFailure $ "Failed to parse: " ++ err
--         Left (err, _) -> assertFailure $ "Failed to parse: " ++ err

-- testScanner_analyzeg :: TestTree
-- testScanner_analyzeg = testCase "analyzeg using IO-enabled parser" $ do
--     let input = Hh.Snippet "GET http://localhost\n"
--     res <- runMaybeT $ Hh.analyzeg input
--     case res of
--         Just s -> do
--             let items = Hh.callItemsg s
--             assertBool "Should have one item" (length items == 1)
--             let item = head items
--                 reqSpeg = Hh.cgRequestSpec item
--             case Hh.requestStruct reqSpeg of
--                 Just _ -> pure ()
--                 Nothing -> assertFailure "Should have parsed request struct via IO"
--         Nothing -> assertFailure "analyzeg returned Nothing"

-- testTypes_presentg :: TestTree
-- testTypes_presentg = testCase "presentg formatting" $ do
--     req <- Prim.parseRequest "POST http://localhost/foo"
--     let reqSpeg = Hh.RequestSpeg
--           { Hh.requestStruct = Just req
--           , Hh.lexedUrl = "http://localhost/foo"
--           , Hh.headersg = Hh.RhsDict Hh.mtHM
--           , Hh.configsg = Hh.RhsDict Hh.mtHM
--           , Hh.payloadg = "{\"foo\":\"bar\"}"
--           }
--         ci = Hh.CallItemg
--           { Hh.cgDeps = []
--           , Hh.cgName = "test"
--           , Hh.cgRequestSpec = reqSpeg
--           , Hh.cgResponseSpec = Nothing
--           }
--         output = Hh.presentg ci
--     assertEqual "Should format correctly" "POST http://localhost/foo\n{\"foo\":\"bar\"}\n" output
