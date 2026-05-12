{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExecutionSpec (spec) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text
import qualified Data.CaseInsensitive as CI
import qualified Data.HashMap.Strict as HM
import           Data.List (isInfixOf)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import           Network.HTTP.Types.Header (ResponseHeaders)

import qualified BEL
import           Hh200.Execution
import           Hh200.Types (RhsDict (..), RequestSquare(..))
import           Hh200.Execution (SubsetResult(..), Side(..), renderRequestQuery, renderRequestForm, renderRequestCookies)


-- | Minimal BEL.Env for testing with only literal parts (no evaluation needed).
-- The dummy request/response are never accessed when rendering R parts.
testEnv :: BEL.Env
testEnv = BEL.Env
  { BEL.storedResponse = undefined
  , BEL.storedRequest = undefined
  , BEL.bindings = HM.empty
  }

spec :: TestTree
spec = testGroup "Execution"
  [ testIsSubmapOfBy
  , testRenderHeadersMap
  , testRenderHeadersMapStitch
  , testRenderHeadersMapMultiPart
  , testObjectSubset
  , testObjectSubsetEmpty
  , testObjectSubsetExtraKey
  , testObjectSubsetDifferentValue
  , testObjectSubsetNested
  , testJsonSubsetEqual
  , testJsonSubsetASubsetOfB
  , testJsonSubsetBSubsetOfA
  , testJsonSubsetIncomparable
  , testJsonSubsetInvalidA
  , testJsonSubsetInvalidBoth
  , testRenderRequestQueryEmpty
  , testRenderRequestQuerySingle
  , testRenderRequestQueryMultiple
  , testRenderRequestFormEmpty
  , testRenderRequestFormSingle
  , testRenderRequestFormMultiple
  , testRenderRequestCookiesEmpty
  , testRenderRequestCookiesSingle
  , testRenderRequestCookiesMultiple
  ]

testIsSubmapOfBy :: TestTree
testIsSubmapOfBy = testCase "isSubmapOfBy: usage control" $ do
    let t1 = HM.fromList [("k1" :: String, "v1" :: String)]
        t2 = HM.fromList [("k1", "v1"), ("k2", "v2")]

    assertBool "all keys in t1 are in t2 with rel" (HM.isSubmapOfBy (==) t1 t2)
    assertBool "fail when not submap" (not $ HM.isSubmapOfBy (const (const False)) t1 t1)

testRenderHeadersMap :: TestTree
testRenderHeadersMap = testCase "renderHeadersMap: literal" $ do
    let input = HM.fromList [("Content-Type", [BEL.R "application/json"])]
    r1 <- renderHeadersMap testEnv (RhsDict input)
    r2 <- renderHeadersMap testEnv (RhsDict HM.empty)

    case HM.lookup (CI.mk "content-type") r1 of
        Just (Aeson.String v) -> do
            assertEqual "empty result"   0 (HM.size r2)
            assertEqual "correct value" "application/json" v
        Just _ ->  assertFailure "expected String value"
        Nothing -> assertFailure "Content-Type key missing"

testRenderHeadersMapStitch :: TestTree
testRenderHeadersMapStitch = testCase "renderHeadersMap: list of literals" $ do
    let input = HM.fromList [("Content-Type", [BEL.R "application", BEL.R "/", BEL.R "json"])]
    r1 <- renderHeadersMap testEnv (RhsDict input)
    case HM.lookup (CI.mk "content-type") r1 of
        Just (Aeson.String v) -> do
            assertEqual "correct value" "application/json" v
        Just _ ->  assertFailure "expected String value"
        Nothing -> assertFailure "Content-Type key missing"

testRenderHeadersMapMultiPart :: TestTree
testRenderHeadersMapMultiPart = testCase "renderHeadersMap: multiple headers" $ do
    let input = HM.fromList
            [ ("Accept", [BEL.R "text/html"])
            , ("X-Custom", [BEL.R "value123"])
            ]
    result <- renderHeadersMap testEnv (RhsDict input)
    assertEqual "two entries" 2 (HM.size result)
    case HM.lookup (CI.mk "accept") result of
        Nothing -> assertFailure "Accept key missing"
        Just (Aeson.String v) -> assertEqual "Accept value" "text/html" v
        Just _ -> assertFailure "expected String value"
    case HM.lookup (CI.mk "x-custom") result of
        Nothing -> assertFailure "X-Custom key missing"
        Just (Aeson.String v) -> assertEqual "X-Custom value" "value123" v
        Just _ -> assertFailure "expected String value"

-- (auto testObject*)
testObjectSubset :: TestTree
testObjectSubset = testCase "objectSubset: identical objects" $ do
    let obj1 = HM.fromList [(Key.fromString "a", Aeson.Number 1), (Key.fromString "b", Aeson.Number 2)]
        obj2 = HM.fromList [(Key.fromString "a", Aeson.Number 1), (Key.fromString "b", Aeson.Number 2)]
    assertBool "identical objects are subsets" $ objectSubset obj1 obj2

testObjectSubsetEmpty :: TestTree
testObjectSubsetEmpty = testCase "objectSubset: empty object is subset of any" $ do
    let obj1 = HM.empty :: HM.HashMap Key.Key Aeson.Value
        obj2 = HM.fromList [(Key.fromString "a", Aeson.Number 1)]
    assertBool "empty is subset" $ objectSubset obj1 obj2

testObjectSubsetExtraKey :: TestTree
testObjectSubsetExtraKey = testCase "objectSubset: extra key fails subset" $ do
    let obj1 = HM.fromList [(Key.fromString "a", Aeson.Number 1), (Key.fromString "b", Aeson.Number 2)]
        obj2 = HM.fromList [(Key.fromString "a", Aeson.Number 1)]
    assertBool "extra key should not be subset" $ not (objectSubset obj1 obj2)

testObjectSubsetDifferentValue :: TestTree
testObjectSubsetDifferentValue = testCase "objectSubset: different value is not subset" $ do
    let obj1 = HM.fromList [(Key.fromString "a", Aeson.Number 1)]
        obj2 = HM.fromList [(Key.fromString "a", Aeson.Number 2)]
    assertBool "different value should not be subset" $ not (objectSubset obj1 obj2)

testObjectSubsetNested :: TestTree
testObjectSubsetNested = testCase "objectSubset: nested objects" $ do
    let inner1 = HM.fromList [(Key.fromString "x", Aeson.Number 1)]
        inner2 = HM.fromList [(Key.fromString "x", Aeson.Number 1), (Key.fromString "y", Aeson.Number 2)]
        obj1 = HM.fromList [(Key.fromString "nested", Aeson.Object (KeyMap.fromHashMap inner1))]
        obj2 = HM.fromList [(Key.fromString "nested", Aeson.Object (KeyMap.fromHashMap inner2))]
    assertBool "nested subset" $ objectSubset obj1 obj2

testJsonSubsetEqual :: TestTree
testJsonSubsetEqual = testCase "jsonSubset: identical JSON objects" $ do
    let inputA = "{\"a\":1,\"b\":2}" :: BS.ByteString
        inputB = "{\"a\":1,\"b\":2}" :: BS.ByteString
    assertEqual "equal objects" Equal (jsonSubset inputA inputB)

testJsonSubsetASubsetOfB :: TestTree
testJsonSubsetASubsetOfB = testCase "jsonSubset: A is subset of B" $ do
    let inputA = "{\"a\":1}" :: BS.ByteString
        inputB = "{\"a\":1,\"b\":2}" :: BS.ByteString
    assertEqual "A subset of B" ASubsetOfB (jsonSubset inputA inputB)

testJsonSubsetBSubsetOfA :: TestTree
testJsonSubsetBSubsetOfA = testCase "jsonSubset: B is subset of A" $ do
    let inputA = "{\"a\":1,\"b\":2}" :: BS.ByteString
        inputB = "{\"a\":1}" :: BS.ByteString
    assertEqual "B subset of A" BSubsetOfA (jsonSubset inputA inputB)

testJsonSubsetIncomparable :: TestTree
testJsonSubsetIncomparable = testCase "jsonSubset: incomparable objects" $ do
    let inputA = "{\"a\":1}" :: BS.ByteString
        inputB = "{\"b\":2}" :: BS.ByteString
    assertEqual "incomparable" Incomparable (jsonSubset inputA inputB)

testJsonSubsetInvalidA :: TestTree
testJsonSubsetInvalidA = testCase "jsonSubset: invalid JSON on side A" $ do
    let inputA = "not json" :: BS.ByteString
        inputB = "{\"a\":1}" :: BS.ByteString
    assertEqual "invalid A" (InvalidJson SideA) (jsonSubset inputA inputB)

testJsonSubsetInvalidBoth :: TestTree
testJsonSubsetInvalidBoth = testCase "jsonSubset: invalid JSON on both sides" $ do
    let inputA = "not json" :: BS.ByteString
        inputB = "also not" :: BS.ByteString
    assertEqual "invalid both" (InvalidJson BothSides) (jsonSubset inputA inputB)

testRenderRequestQueryEmpty :: TestTree
testRenderRequestQueryEmpty = testCase "renderRequestQuery: empty input" $ do
    result <- renderRequestQuery testEnv Nothing
    assertEqual "empty query" "" result

testRenderRequestQuerySingle :: TestTree
testRenderRequestQuerySingle = testCase "renderRequestQuery: single param" $ do
    let input = RhsDict $ HM.fromList [("key", [BEL.R "value"])]
    result <- renderRequestQuery testEnv (Just (RequestSquareQuery input))
    assertEqual "single param" "key=value" result

testRenderRequestQueryMultiple :: TestTree
testRenderRequestQueryMultiple = testCase "renderRequestQuery: multiple params" $ do
    let input = RhsDict $ HM.fromList
            [ ("a", [BEL.R "1"])
            , ("b", [BEL.R "2"])
            ]
    result <- renderRequestQuery testEnv (Just (RequestSquareQuery input))
    assertBool "contains both params" (("a=1" `isInfixOf` result) && ("b=2" `isInfixOf` result))
    assertBool "has ampersand" ('&' `elem` result)

testRenderRequestFormEmpty :: TestTree
testRenderRequestFormEmpty = testCase "renderRequestForm: empty input" $ do
    result <- renderRequestForm testEnv Nothing
    assertEqual "empty form" "" result

testRenderRequestFormSingle :: TestTree
testRenderRequestFormSingle = testCase "renderRequestForm: single field" $ do
    let input = RhsDict $ HM.fromList [("username", [BEL.R "alice"])]
    result <- renderRequestForm testEnv (Just (RequestSquareForm input))
    assertEqual "single field" "username=alice" result

testRenderRequestFormMultiple :: TestTree
testRenderRequestFormMultiple = testCase "renderRequestForm: multiple fields" $ do
    let input = RhsDict $ HM.fromList
            [ ("name", [BEL.R "John"])
            , ("age", [BEL.R "30"])
            ]
    result <- renderRequestForm testEnv (Just (RequestSquareForm input))
    assertBool "contains both fields" (("name=John" `isInfixOf` result) && ("age=30" `isInfixOf` result))
    assertBool "has ampersand" ('&' `elem` result)

testRenderRequestCookiesEmpty :: TestTree
testRenderRequestCookiesEmpty = testCase "renderRequestCookies: empty input" $ do
    result <- renderRequestCookies testEnv Nothing
    assertEqual "empty cookies" [] result

testRenderRequestCookiesSingle :: TestTree
testRenderRequestCookiesSingle = testCase "renderRequestCookies: single cookie" $ do
    let input = RhsDict $ HM.fromList [("session", [BEL.R "abc123"])]
    result <- renderRequestCookies testEnv (Just (RequestSquareCookies input))
    assertBool "has cookie header" (case result of [("Cookie", v)] -> "session=abc123" `isInfixOf` BS.unpack v; _ -> False)

testRenderRequestCookiesMultiple :: TestTree
testRenderRequestCookiesMultiple = testCase "renderRequestCookies: multiple cookies" $ do
    let input = RhsDict $ HM.fromList
            [ ("sid", [BEL.R "xyz"])
            , ("uid", [BEL.R "42"])
            ]
    result <- renderRequestCookies testEnv (Just (RequestSquareCookies input))
    assertBool "has cookie header" (case result of [("Cookie", v)] -> ("sid=xyz" `isInfixOf` BS.unpack v) && ("uid=42" `isInfixOf` BS.unpack v); _ -> False)
