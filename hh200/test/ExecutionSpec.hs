{-# LANGUAGE OverloadedStrings #-}

module ExecutionSpec (spec) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.ByteString.Char8    as BS
import qualified Data.CaseInsensitive     as CI
import qualified Data.HashMap.Strict      as HM
import qualified Data.Text                as Text

import qualified BEL
import           Hh200.Execution          (rhsDictToResponseHeaders)
import           Hh200.Types              (RhsDict (..))
import           Network.HTTP.Types.Header (ResponseHeaders)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

hdr :: String -> String -> (CI.CI BS.ByteString, BS.ByteString)
hdr k v = (CI.mk (BS.pack k), BS.pack v)

-- Build a RhsDict with only literal (R) parts, as produced by the parser for
-- plain string values.
fromLiterals :: [(String, String)] -> RhsDict
fromLiterals pairs =
    RhsDict $ HM.fromList [ (k, [BEL.R (Text.pack v)]) | (k, v) <- pairs ]

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: TestTree
spec = testGroup "Execution"
    [ testRhsDictToResponseHeaders
    , testRhsDictToResponseHeadersCaseInsensitive
    , testRhsDictToResponseHeadersEmpty
    , testRhsDictToResponseHeadersMultiPartValue
    ]

-- | A single header entry round-trips through rhsDictToResponseHeaders.
testRhsDictToResponseHeaders :: TestTree
testRhsDictToResponseHeaders = testCase "rhsDictToResponseHeaders: single literal header" $ do
    let dict = fromLiterals [("content-type", "application/json")]
        result :: ResponseHeaders
        result = rhsDictToResponseHeaders dict
    assertEqual "one entry" 1 (length result)
    assertBool "header key present" (CI.mk "content-type" `elem` map fst result)
    assertBool "header value present" ("application/json" `elem` map snd result)

-- | Header name comparisons should be case-insensitive (http-types CI).
testRhsDictToResponseHeadersCaseInsensitive :: TestTree
testRhsDictToResponseHeadersCaseInsensitive = testCase "rhsDictToResponseHeaders: key is case-insensitive" $ do
    let dict   = fromLiterals [("Content-Type", "text/plain")]
        result = rhsDictToResponseHeaders dict
    assertBool "CI key matches lowercase"
        (CI.mk "content-type" `elem` map fst result)
    assertBool "CI key matches uppercase"
        (CI.mk "CONTENT-TYPE" `elem` map fst result)

-- | Empty RhsDict produces empty ResponseHeaders.
testRhsDictToResponseHeadersEmpty :: TestTree
testRhsDictToResponseHeadersEmpty = testCase "rhsDictToResponseHeaders: empty dict" $ do
    let result = rhsDictToResponseHeaders (RhsDict HM.empty)
    assertEqual "empty result" [] result

-- | Multiple Part tokens in one value are concatenated.
testRhsDictToResponseHeadersMultiPartValue :: TestTree
testRhsDictToResponseHeadersMultiPartValue = testCase "rhsDictToResponseHeaders: multi-part value concatenated" $ do
    let dict = RhsDict $ HM.fromList
                  [ ("x-token", [BEL.R "Bearer ", BEL.L "someVar"]) ]
        result = rhsDictToResponseHeaders dict
    case lookup (CI.mk "x-token") result of
        Nothing  -> assertFailure "x-token header missing"
        Just val -> assertEqual "parts concatenated" "Bearer someVar" val
