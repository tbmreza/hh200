{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ScannerSpec where

import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except (runExceptT)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap

import Hh200.Types as Hh
import Hh200.Scanner as Hh

spec :: TestTree
spec = testGroup "Scanner and Parser"
  [ testScanner_lr
  , testScanner_lrMustache
  , testScanner_lrPost
  , testScanner_lr3lineAsserts
  , testScanner_lr3lineAssertsNoGT
  , testScanner_lrPostMultiline
  , testScanner_lrInvalid
  , testScanner_lrEmpty
  , testScanner_lrResponseOrder
  , testScanner_lrRequestConfigs
  ]

testScanner_lr :: TestTree
testScanner_lr = testCase "lexer and parser" $ do
    let tokens = Hh.alexScanTokens "POST http://localhost:9999/echo.php\nAuthorization: Bearer \nTest: $.data.id\nUser-Agent: \"lite\""
    res <- runExceptT (Hh.parse tokens)
    case res of
        Right _ -> pure ()
        Left _ -> assertFailure $ show tokens

testScanner_lrMustache :: TestTree
testScanner_lrMustache = testCase "lexer and parser for valid mustached" $ do
    let input = "POST http://httpbin.org/post&unused={{100}}"
        tokens = Hh.alexScanTokens input
    res <- runExceptT (Hh.parse tokens)
    case res of
        Right _ -> pure ()
        Left _ -> assertFailure $ "Failed to parse: " ++ show tokens

testScanner_lrPost :: TestTree
testScanner_lrPost = testCase "lexer and parser for POST" $ do
    let input = "POST http://httpbin.org/post\nContent-Type: application/json\n\n{ \"foo\": \"bar\", \"baz\": 123 }"
        tokens = Hh.alexScanTokens input
    res <- runExceptT (Hh.parse tokens)
    case res of
        Right _ -> pure ()
        Left _ -> assertFailure $ "Failed to parse: " ++ show tokens

testScanner_lr3lineAsserts :: TestTree
testScanner_lr3lineAsserts = testCase "lexer and parser for POST" $ do
    let input = "GET http://httpbin.org/post\n[Asserts]\n>true\n>true\n>true"
        tokens = Hh.alexScanTokens input
    res <- runExceptT (Hh.parse tokens)
    case res of
        Right _ -> pure ()
        Left _ -> assertFailure $ "Failed to parse: " ++ show tokens

testScanner_lr3lineAssertsNoGT :: TestTree
testScanner_lr3lineAssertsNoGT = testCase "lexer and parser for Asserts without >" $ do
    let input = "GET http://httpbin.org/post\n[Asserts]\ntrue\ntrue\ntrue"
        tokens = Hh.alexScanTokens input
    res <- runExceptT (Hh.parse tokens)
    case res of
        Right _ -> pure ()
        Left _ -> assertFailure $ "Failed to parse: " ++ show tokens

testScanner_lrPostMultiline :: TestTree
testScanner_lrPostMultiline = testCase "lexer and parser for POST with multiline JSON" $ do
    let input = "POST http://httpbin.org/post\nContent-Type: application/json\n\n{\n  \"foo\": \"bar\",\n  \"baz\": 123\n}"
        tokens = Hh.alexScanTokens input
    res <- runExceptT (Hh.parse tokens)
    case res of
        Right _ -> pure ()
        Left _ -> assertFailure $ "Failed to parse: " ++ show tokens

testScanner_lrInvalid :: TestTree
testScanner_lrInvalid = testCase "lexer and parser for invalid input" $ do
    let input = "INVALID http://httpbin.org/post"
        tokens = Hh.alexScanTokens input
    res <- runExceptT (Hh.parse tokens)
    case res of
        Right _ -> assertFailure "Should have failed to parse"
        Left _ -> pure ()

testScanner_lrEmpty :: TestTree
testScanner_lrEmpty = testCase "lexer and parser for empty input" $ do
    let input = ""
        tokens = Hh.alexScanTokens input
    res <- runExceptT (Hh.parse tokens)
    case res of
        Right _ -> assertFailure "Should have failed to parse"
        Left _ -> pure ()

testScanner_lrResponseOrder :: TestTree
testScanner_lrResponseOrder = testGroup "lexer and parser for response block order"
    [ testCase "Captures before Asserts" $ do
        let input = "GET http://localhost\n\nHTTP 200\n[Captures]\nfoo: bar\n\n[Asserts]\n> true\n"
            tokens = Hh.alexScanTokens input
        res <- runExceptT (Hh.parse tokens)
        case res of
            Right action -> do
                res2 <- runExceptT action
                case res2 of
                    Right s -> do
                        let ci = head (Hh.callItems s)
                        case Hh.ciResponseSpec ci of
                            Just rs -> do
                                assertBool "Has captures" $ not (Hh.mtHM == (let Hh.RhsDict hm = Hh.captures rs in hm))
                                assertBool "Has asserts" $ not (null (Hh.asserts rs))
                            Nothing -> assertFailure "Should have response spec"
                    Left (err, _) -> assertFailure $ "Failed to parse: " ++ err
            Left (err, _) -> assertFailure $ "Failed to parse: " ++ err
    , testCase "Asserts before Captures" $ do
        let input = "GET http://localhost\n\nHTTP 200\n[Asserts]\n> true\n\n[Captures]\nfoo: bar\n"
            tokens = Hh.alexScanTokens input
        res <- runExceptT (Hh.parse tokens)
        case res of
            Right action -> do
                res2 <- runExceptT action
                case res2 of
                    Right s -> do
                        let ci = head (Hh.callItems s)
                        case Hh.ciResponseSpec ci of
                            Just rs -> do
                                assertBool "Has captures" $ not (Hh.mtHM == (let Hh.RhsDict hm = Hh.captures rs in hm))
                                assertBool "Has asserts" $ not (null (Hh.asserts rs))
                            Nothing -> assertFailure "Should have response spec"
                    Left (err, _) -> assertFailure $ "Failed to parse: " ++ err
            Left (err, _) -> assertFailure $ "Failed to parse: " ++ err
    ]

testScanner_lrRequestConfigs :: TestTree
testScanner_lrRequestConfigs = testCase "lexer and parser for request configs" $ do
    let input = "GET http://localhost\nAuthorization: Bearer token\n[Configs]\nretry: 3\n\n{ \"body\": \"here\" }\n"
        tokens = Hh.alexScanTokens input
    res <- runExceptT (Hh.parse tokens)
    case res of
        Right action -> do
            res2 <- runExceptT action
            case res2 of
                Right s -> do
                    let ci = head (Hh.callItems s)
                        rs = Hh.ciRequestSpec ci
                    assertBool "Has configs" $ not (Hh.mtHM == (let Hh.RhsDict hm = Hh.configs rs in hm))
                    assertEqual "Payload correct" "{ \"body\": \"here\" }" (Hh.payload rs)
                Left (err, _) -> assertFailure $ "Failed to parse: " ++ err
        Left (err, _) -> assertFailure $ "Failed to parse: " ++ err
