{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExecutionSpec (spec) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Aeson               as Aeson
import qualified Data.ByteString.Char8    as BS
import qualified Data.CaseInsensitive     as CI
import qualified Data.HashMap.Strict      as HM
import qualified Data.Aeson.Key           as Key
import qualified Data.Aeson.KeyMap        as KeyMap
import qualified Data.Text                as Text

import qualified BEL
import           Hh200.Execution
import           Hh200.Types              (RhsDict (..))
import           Hh200.Execution         (SubsetResult(..), Side(..))
import           Network.HTTP.Types.Header (ResponseHeaders)

hdr :: String -> String -> (CI.CI BS.ByteString, BS.ByteString)
hdr k v = (CI.mk (BS.pack k), BS.pack v)

-- Build a RhsDict with only literal (R) parts, as produced by the parser for
-- plain string values.
fromLiterals :: [(String, String)] -> RhsDict
fromLiterals pairs =
    RhsDict $ HM.fromList [ (Text.pack k, [BEL.R (Text.pack v)]) | (k, v) <- pairs ]

-- | Minimal BEL.Env for testing with only literal parts (no evaluation needed).
-- The dummy request/response are never accessed when rendering R parts.
testEnv :: BEL.Env
testEnv = BEL.Env
    { BEL.storedResponse = undefined
    , BEL.storedRequest = undefined
    , BEL.bindings = HM.empty
    }

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

spec :: TestTree
spec = testGroup "Execution"
    [ testIsSubmapOfBy
    , testRenderHeadersMap
    , testRenderHeadersMapEmpty
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
    ]

-- testRhsDictToResponseHeaders :: TestTree
-- testRhsDictToResponseHeaders = testCase "rhsDictToResponseHeaders: single literal header" $ do
--     let dict = fromLiterals [("Content-Type", "application/json")]
--         result :: ResponseHeaders = rhsDictToResponseHeaders dict
--     assertEqual "one entry" 1 (length result)
--     assertBool "header key present" (CI.mk "content-type" `elem` map fst result)
--     assertBool "header value present" ("application/json" `elem` map snd result)
--
-- testRhsDictToResponseHeadersCaseInsensitive :: TestTree
-- testRhsDictToResponseHeadersCaseInsensitive = testCase "rhsDictToResponseHeaders: key is case-insensitive" $ do
--     let dict   = fromLiterals [("Content-Type", "text/plain")]
--         result = rhsDictToResponseHeaders dict
--     assertBool "CI key matches lowercase"
--         (CI.mk "content-type" `elem` map fst result)
--     assertBool "CI key matches uppercase"
--         (CI.mk "CONTENT-TYPE" `elem` map fst result)
--
-- testRhsDictToResponseHeadersEmpty :: TestTree
-- testRhsDictToResponseHeadersEmpty = testCase "rhsDictToResponseHeaders: empty dict" $ do
--     let result = rhsDictToResponseHeaders (RhsDict HM.empty)
--     assertEqual "empty result" [] result
--
-- -- | Multiple Part tokens in one value are concatenated.
-- testRhsDictToResponseHeadersMultiPartValue :: TestTree
-- testRhsDictToResponseHeadersMultiPartValue = testCase "rhsDictToResponseHeaders: multi-part value concatenated" $ do
--     let dict = RhsDict $ HM.fromList
--                   [ ("x-token", [BEL.R "Bearer ", BEL.L "someVar"]) ]
--         result = rhsDictToResponseHeaders dict
--     case lookup (CI.mk "x-token") result of
--         Nothing  -> assertFailure "x-token header missing"
--         -- Just val -> assertEqual "parts concatenated" "Bearer someVar" val
--         Just val -> assertEqual "parts concatenated" "Bearer " val  -- ??:

testIsSubmapOfBy :: TestTree
testIsSubmapOfBy = testCase "isSubmapOfBy: nothing to render, subset check" $ do
    let t1 = HM.fromList [("k1" :: String, "v1" :: String)]
        t2 = HM.fromList [("k1", "v1"), ("k2", "v2")]

    assertBool "all keys in t1 are in t2 with rel" (HM.isSubmapOfBy (==) t1 t2)
    assertBool "fail when not submap" (not $ HM.isSubmapOfBy (const (const False)) t1 t1)

testRenderHeadersMap :: TestTree
testRenderHeadersMap = testCase "renderHeadersMap: single literal header" $ do
    let input = HM.fromList [("Content-Type", [BEL.R "application/json"])]
    result <- renderHeadersMap testEnv (RhsDict input)
    assertEqual "one entry" 1 (HM.size result)
    case HM.lookup (CI.mk "content-type") result of
        Nothing -> assertFailure "Content-Type key missing"
        Just (Aeson.String v) -> assertEqual "correct value" "application/json" v
        Just _ -> assertFailure "expected String value"

testRenderHeadersMapEmpty :: TestTree
testRenderHeadersMapEmpty = testCase "renderHeadersMap: empty input" $ do
    result <- renderHeadersMap testEnv (RhsDict HM.empty)
    assertEqual "empty result" 0 (HM.size result)

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
