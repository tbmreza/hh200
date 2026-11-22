{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ScannerSpec where

import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad.Trans.Except (runExceptT)
import qualified Data.HashMap.Strict as HM
import qualified BEL

import Hh200.Types as Hh
import Hh200.Scanner as Hh

-- ??: review RhsDict, Content-Type rhs ideally doesn't require ""
--
-- GET http://localhost:9999/api/login
-- Content-Type: "application/json"

scanTokens :: String -> IO [Hh.Token]
scanTokens input = case Hh.alexScanTokens input of
    Left e -> assertFailure ("lex error: " ++ Hh.lexErrorMsg e)
    Right ts -> pure ts

spec :: TestTree
spec = testGroup "Scanner and Parser"
  [ testScanner_lrBasic
  , testScanner_lrMustache
  , testScanner_lrInvalid
  , testScanner_lrResponseOrder
  , testScanner_lrRequestConfigs
  , testScanner_vars
  ]

testScanner_lrBasic :: TestTree
testScanner_lrBasic = testGroup "Lexer and Parser - Basic"
  [ testCase "Basic request with headers" $ do
      tokens <- scanTokens "POST http://localhost:9999/echo.php\nAuthorization: Bearer \nTest: $.data.id\nUser-Agent: \"lite\""
      res <- runExceptT (Hh.parse tokens)
      case res of
          Right _ -> pure ()
          Left _ -> assertFailure $ show tokens

  , testCase "POST with JSON body" $ do
      let input = "POST http://httpbin.org/post\nContent-Type: application/json\n\n{ \"foo\": \"bar\", \"baz\": 123 }"
      tokens <- scanTokens input
      res <- runExceptT (Hh.parse tokens)
      case res of
          Right _ -> pure ()
          Left _ -> assertFailure $ "Failed to parse: " ++ show tokens

  , testCase "POST with multiline JSON" $ do
      let input = "POST http://httpbin.org/post\nContent-Type: application/json\n\n{\n  \"foo\": \"bar\",\n  \"baz\": 123\n}"
      tokens <- scanTokens input
      res <- runExceptT (Hh.parse tokens)
      case res of
          Right _ -> pure ()
          Left _ -> assertFailure $ "Failed to parse: " ++ show tokens

  , testCase "TRACE is an accepted method" $ do
      tokens <- scanTokens "TRACE http://localhost:9999/echo\nHTTP 200"
      res <- runExceptT (Hh.parse tokens)
      case res of
          Right _ -> pure ()
          Left _ -> assertFailure "TRACE failed to parse"

  , testCase "CONNECT is an accepted method" $ do
      tokens <- scanTokens "CONNECT http://localhost:9999/echo\nHTTP 200"
      res <- runExceptT (Hh.parse tokens)
      case res of
          Right _ -> pure ()
          Left _ -> assertFailure "CONNECT failed to parse"
  ]

testScanner_lrMustache :: TestTree
testScanner_lrMustache = testGroup "Lexer and Parser - Mustache"
  [ testCase "Template variable renders" $ do
      let input = "POST http://httpbin.org/post&unused={{100}}"
      tokens <- scanTokens input
      res <- runExceptT (Hh.parse tokens)
      case res of
          Right _ -> pure ()
          Left _ -> assertFailure $ "Failed to parse: " ++ show tokens
  ]

testScanner_lrInvalid :: TestTree
testScanner_lrInvalid = testGroup "Lexer and Parser - Errors"
  [ testCase "Invalid method fails" $ do
      let input = "INVALID http://httpbin.org/post"
      tokens <- scanTokens input
      res <- runExceptT (Hh.parse tokens)
      case res of
          Right _ -> assertFailure "Should have failed to parse"
          Left _ -> pure ()

  , testCase "Empty input fails" $ do
      let input = ""
      tokens <- scanTokens input
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
      tokens <- scanTokens input
      res <- runExceptT (Hh.parse tokens)
      case res of
          Right s -> do
              let ci = head (Hh.callItems s)
              case Hh.ciResponseSpec ci of
                  Just rs -> assertResponseSquares rs
                  Nothing -> assertFailure "Should have response spec"
          Left (err, _) -> assertFailure $ "Failed to parse: " ++ err

  , testCase "Asserts before Captures" $ do
      let input = "GET http://localhost\n\nHTTP 200\n[Asserts]\n> true\n\n[Captures]\nfoo: bar\n"
      tokens <- scanTokens input
      res <- runExceptT (Hh.parse tokens)
      case res of
          Right s -> do
              let ci = head (Hh.callItems s)
              case Hh.ciResponseSpec ci of
                  Just rs -> assertResponseSquares rs
                  Nothing -> assertFailure "Should have response spec"
          Left (err, _) -> assertFailure $ "Failed to parse: " ++ err
  ]

testScanner_lrRequestConfigs :: TestTree
testScanner_lrRequestConfigs = testCase "lexer and parser for request configs" $ do
    let input = "GET http://localhost\nAuthorization: Bearer token\n[Configs]\nretry: 3\n\n{ \"body\": \"here\" }\n"
    tokens <- scanTokens input
    res <- runExceptT (Hh.parse tokens)
    case res of
        Right s -> do
            let ci = head (Hh.callItems s)
                rs = Hh.ciRequestSpec ci
            case Hh.sqConfigs (Hh.rqSquares rs) of
                Just (Hh.RequestSquareConfigs (Hh.RhsDict cfgs)) ->
                    assertBool "Has configs" (not (HM.null cfgs))
                _ -> assertFailure "Expected Configs request square"
            assertEqual "Payload correct" "{ \"body\": \"here\" }" (Hh.rqBody rs)
        Left (err, _) -> assertFailure $ "Failed to parse: " ++ err

-- | Phase 1: the @[Vars]@ request square.
testScanner_vars :: TestTree
testScanner_vars = testGroup "[Vars] square"
  [ testCase "Vars lexes as a keyword" $ do
      tokens <- scanTokens "[Vars]"
      assertBool "KW_VARS token" (any isVars tokens)
  , testCase "a [Vars] block parses into sqVars" $ do
      rs <- firstRequest resendLike
      case Hh.sqVars (Hh.rqSquares rs) of
          Just (Hh.RequestSquareVars (Hh.RhsDict d)) -> do
              assertEqual "names" ["IMG", "KIND"] (sortText (HM.keys d))
              assertEqual "IMG value" [BEL.R "base64-file,photo.png"] (HM.lookupDefault [] "IMG" d)
          _ -> assertFailure "expected a Vars square"
      assertBool "body kept" (not (null (Hh.rqBody rs)))
  , testCase "[Vars] sits beside other request squares" $ do
      rs <- firstRequest "POST http://localhost/x\n[Query]\nq: 1\n[Vars]\nA: b\n"
      assertBool "query" (isJustSq (Hh.sqQuery (Hh.rqSquares rs)))
      assertBool "vars" (isJustSq (Hh.sqVars (Hh.rqSquares rs)))
  , testCase "[Vars] names are definitions for the LSP index" $ do
      r <- Hh.analyze (Hh.Snippet "POST http://localhost/x\n[Vars]\nIMG: base64-file,a.png\n")
      case r of
          Left err -> assertFailure err
          Right s -> assertBool "IMG indexed" ("IMG" `elem` map fst (Hh.ixCaptures (Hh.index s)))
  ]
  where
    isVars (Hh.KW_VARS _) = True
    isVars _ = False
    isJustSq = maybe False (const True)
    sortText = foldr insert []
    insert x [] = [x]
    insert x (y : ys) = if x <= y then x : y : ys else y : insert x ys
    resendLike = concat
        [ "POST https://api.resend.com/emails\n"
        , "Authorization: Bearer {{RESEND_API_KEY}}\n"
        , "Content-Type: application/json\n"
        , "[Vars]\n"
        , "IMG: base64-file,photo.png\n"
        , "KIND: image/png\n"
        , "{ \"attachments\": [ { \"filename\": \"photo.png\", \"content\": \"{{IMG}}\" } ] }\n"
        , "HTTP 200\n"
        ]
    firstRequest src = do
        tokens <- scanTokens src
        res <- runExceptT (Hh.parse tokens)
        case res of
            Right s -> case Hh.callItems s of
                (ci : _) -> pure (Hh.ciRequestSpec ci)
                [] -> assertFailure "no call items" >> fail "unreachable"
            Left (err, _) -> assertFailure ("Failed to parse: " ++ err) >> fail "unreachable"
