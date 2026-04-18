{-# LANGUAGE ScopedTypeVariables #-}

module MustacheSpec (spec) where

import Test.Tasty
import Test.Tasty.HUnit

import Hh200.Scanner (hasBalancedMustache)

spec :: TestTree
spec = testGroup "hasBalancedMustache"
    [ testNoMustache
    , testBalancedSingle
    , testBalancedMultiple
    , testUnbalancedOpen
    , testUnbalancedClose
    , testUnbalancedMultipleOpens
    , testEmptyString
    ]

testNoMustache :: TestTree
testNoMustache = testCase "no mustache returns False" $ do
    assertBool "plain URL" $ not $ hasBalancedMustache "https://example.com/api/resource"
    assertBool "empty braces" $ not $ hasBalancedMustache "{}"
    assertBool "single brace" $ not $ hasBalancedMustache "{"

testBalancedSingle :: TestTree
testBalancedSingle = testCase "single balanced pair returns True" $ do
    assertBool "simple pair" $ hasBalancedMustache "{{.foo}}"
    assertBool "pair in URL" $ hasBalancedMustache "https://api/{{id}}/resource"

testBalancedMultiple :: TestTree
testBalancedMultiple = testCase "multiple balanced pairs returns True" $ do
    assertBool "two pairs" $ hasBalancedMustache "{{foo}}{{bar}}"
    assertBool "nested pairs" $ hasBalancedMustache "{{foo}} and {{bar}}"

testUnbalancedOpen :: TestTree
testUnbalancedOpen = testCase "unbalanced open returns False" $ do
    assertBool "single {{" $ not $ hasBalancedMustache "{{foo"
    assertBool "triple brace" $ not $ hasBalancedMustache "{{{foo}}"
    assertBool "open then close mismatched" $ not $ hasBalancedMustache "{{foo}}}"

testUnbalancedClose :: TestTree
testUnbalancedClose = testCase "unbalanced close returns False" $ do
    assertBool "single }}" $ not $ hasBalancedMustache "foo}}"
    assertBool "extra close" $ not $ hasBalancedMustache "{{foo}}}}"

testUnbalancedMultipleOpens :: TestTree
testUnbalancedMultipleOpens = testCase "multiple opens, single close returns False" $ do
    assertBool "two opens, one close" $ not $ hasBalancedMustache "{{foo}}{{bar}"
    assertBool "three opens" $ not $ hasBalancedMustache "{{{foo}}"

testEmptyString :: TestTree
testEmptyString = testCase "empty string returns False" $
    assertBool "empty" $ not $ hasBalancedMustache ""