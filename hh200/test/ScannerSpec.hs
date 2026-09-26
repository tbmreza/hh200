{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ScannerSpec where

import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad.Trans.Except (runExceptT)
import qualified Data.HashMap.Strict as HM

import Hh200.Types as Hh
import Hh200.Scanner as Hh

spec :: TestTree
spec = testGroup "Scanner and Parser"
  [ testScanner_lrBasic
  , testScanner_lrMustache
  , testScanner_lrInvalid
  , testScanner_lrResponseOrder
  , testScanner_lrRequestConfigs
  ]

testScanner_lrBasic :: TestTree
testScanner_lrBasic = testGroup "Lexer and Parser - Basic"
  [ testCase "Basic request with headers" $ do
      let tokens = Hh.alexScanTokens "POST http://localhost:9999/echo.php\nAuthorization: Bearer \nTest: $.data.id\nUser-Agent: \"lite\""
      res <- runExceptT (Hh.parse tokens)
      case res of
          Right _ -> pure ()
          Left _ -> assertFailure $ show tokens

  , testCase "POST with JSON body" $ do
      let input = "POST http://httpbin.org/post\nContent-Type: application/json\n\n{ \"foo\": \"bar\", \"baz\": 123 }"
      let tokens = Hh.alexScanTokens input
      res <- runExceptT (Hh.parse tokens)
      case res of
          Right _ -> pure ()
          Left _ -> assertFailure $ "Failed to parse: " ++ show tokens

  , testCase "POST with multiline JSON" $ do
      let input = "POST http://httpbin.org/post\nContent-Type: application/json\n\n{\n  \"foo\": \"bar\",\n  \"baz\": 123\n}"
      let tokens = Hh.alexScanTokens input
      res <- runExceptT (Hh.parse tokens)
      case res of
          Right _ -> pure ()
          Left _ -> assertFailure $ "Failed to parse: " ++ show tokens
  ]

testScanner_lrMustache :: TestTree
testScanner_lrMustache = testGroup "Lexer and Parser - Mustache"
  [ testCase "Template variable renders" $ do
      let input = "POST http://httpbin.org/post&unused={{100}}"
      let tokens = Hh.alexScanTokens input
      res <- runExceptT (Hh.parse tokens)
      case res of
          Right _ -> pure ()
          Left _ -> assertFailure $ "Failed to parse: " ++ show tokens
  ]

testScanner_lrInvalid :: TestTree
testScanner_lrInvalid = testGroup "Lexer and Parser - Errors"
  [ testCase "Invalid method fails" $ do
      let input = "INVALID http://httpbin.org/post"
      let tokens = Hh.alexScanTokens input
      res <- runExceptT (Hh.parse tokens)
      case res of
          Right _ -> assertFailure "Should have failed to parse"
          Left _ -> pure ()

  , testCase "Empty input fails" $ do
      let input = ""
      let tokens = Hh.alexScanTokens input
      res <- runExceptT (Hh.parse tokens)
      case res of
          Right _ -> assertFailure "Should have failed to parse"
          Left _ -> pure ()
  ]

assertResponseSquares :: Hh.ResponseSpec -> IO ()
assertResponseSquares rs = case Hh.rpSquares rs of
    (Just (Hh.ResponseSquareCaptures (Hh.RhsDict caps)), Just (Hh.ResponseSquareAsserts asserts)) -> do
        assertBool "Has captures" (not (HM.null caps))
        assertBool "Has asserts" (not (null asserts))
    _ -> assertFailure "Expected both Captures and Asserts response squares"

testScanner_lrResponseOrder :: TestTree
testScanner_lrResponseOrder = testGroup "lexer and parser for response block order"
  [ testCase "Captures before Asserts" $ do
      let input = "GET http://localhost\n\nHTTP 200\n[Captures]\nfoo: bar\n\n[Asserts]\n> true\n"
      let tokens = Hh.alexScanTokens input
      res <- runExceptT (Hh.parse tokens)
      case res of
          Right action -> do
              res2 <- runExceptT action
              case res2 of
                  Right s -> do
                      let ci = head (Hh.callItems s)
                      case Hh.ciResponseSpec ci of
                          Just rs -> assertResponseSquares rs
                          Nothing -> assertFailure "Should have response spec"
                  Left (err, _) -> assertFailure $ "Failed to parse: " ++ err
          Left (err, _) -> assertFailure $ "Failed to parse: " ++ err

  , testCase "Asserts before Captures" $ do
      let input = "GET http://localhost\n\nHTTP 200\n[Asserts]\n> true\n\n[Captures]\nfoo: bar\n"
      let tokens = Hh.alexScanTokens input
      res <- runExceptT (Hh.parse tokens)
      case res of
          Right action -> do
              res2 <- runExceptT action
              case res2 of
                  Right s -> do
                      let ci = head (Hh.callItems s)
                      case Hh.ciResponseSpec ci of
                          Just rs -> assertResponseSquares rs
                          Nothing -> assertFailure "Should have response spec"
                  Left (err, _) -> assertFailure $ "Failed to parse: " ++ err
          Left (err, _) -> assertFailure $ "Failed to parse: " ++ err
  ]

testScanner_lrRequestConfigs :: TestTree
testScanner_lrRequestConfigs = testCase "lexer and parser for request configs" $ do
    let input = "GET http://localhost\nAuthorization: Bearer token\n[Configs]\nretry: 3\n\n{ \"body\": \"here\" }\n"
    let tokens = Hh.alexScanTokens input
    res <- runExceptT (Hh.parse tokens)
    case res of
        Right action -> do
            res2 <- runExceptT action
            case res2 of
                Right s -> do
                    let ci = head (Hh.callItems s)
                        rs = Hh.ciRequestSpec ci
                    case Hh.rqSquares rs of
                        (Just (Hh.RequestSquareConfigs (Hh.RhsDict cfgs)), _, _, _, _) ->
                            assertBool "Has configs" (not (HM.null cfgs))
                        _ -> assertFailure "Expected Configs request square"
                    assertEqual "Payload correct" "{ \"body\": \"here\" }" (Hh.rqBody rs)
                Left (err, _) -> assertFailure $ "Failed to parse: " ++ err
        Left (err, _) -> assertFailure $ "Failed to parse: " ++ err
