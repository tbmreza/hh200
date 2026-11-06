{-# LANGUAGE OverloadedStrings #-}

-- Tests for `validate_script`: the pure-ish lexer + parser validation and the
-- MCP wire shape it produces.
module ValidateSpec where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Aeson (Value, encode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)

import Hh200.Validate
import qualified Hh200.McpServer as Mcp

spec :: TestTree
spec = testGroup "Validate"
  [ testCase "valid script validates" $ do
      r <- validateScript "GET http://localhost:9999/echo\nHTTP 200"
      case r of
          Right _ -> pure ()
          Left diags -> assertFailure ("unexpected diagnostics: " ++ show diags)

  , testCase "invalid method reports a parse error at line 1" $ do
      r <- validateScript "INVALID http://localhost:9999/echo"
      case r of
          Left [d] -> do
              assertEqual "code" ParseError (diagCode d)
              assertEqual "line" 1 (diagLine d)
              assertBool "message non-empty" (not (null (diagMessage d)))
          _ -> assertFailure "expected exactly one parse diagnostic"

  , testCase "unbalanced braces reports a lexical error" $ do
      r <- validateScript "POST http://localhost:9999/echo\n{\n  \"a\": 1"
      case r of
          Left [d] -> assertEqual "code" LexicalError (diagCode d)
          _ -> assertFailure "expected exactly one lexical diagnostic"

  , testCase "validateScriptResult shapes a clean script as ok=true" $ do
      payload <- Mcp.validateScriptResult "GET http://localhost:9999/echo\nHTTP 200"
      assertBool "ok=true" (jsonContains "\"ok\":true" payload)

  , testCase "validateScriptResult shapes a failure as ok=false + errors" $ do
      payload <- Mcp.validateScriptResult "INVALID http://localhost:9999/echo"
      assertBool "ok=false" (jsonContains "\"ok\":false" payload)
      assertBool "code=parse_error" (jsonContains "parse_error" payload)
      assertBool "has line" (jsonContains "line" payload)
      assertBool "has col" (jsonContains "col" payload)
      assertBool "has hint" (jsonContains "hint" payload)
  ]

jsonContains :: String -> Value -> Bool
jsonContains needle v =
    T.pack needle `T.isInfixOf` decodeUtf8 (BL.toStrict (encode v))
