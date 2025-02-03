import Test.Tasty
import Test.Tasty.HUnit
import L
import P

import Hh200.Cli (interpret)

main = defaultMain t

t :: TestTree
t = testGroup "syntax" [

    testCase "" $ do
        let input = "print 201;"
        let tokens = alexScanTokens input
        let ast = parse tokens
        print ast

  , testCase "" $ do
        let input = "POST https://example.com"
        let tokens = alexScanTokens input
        let ast = parse tokens
        print ast

  , testCase "" $ do
        -- let input = "POST http://localhost:9999/user/12"
        let input = "get http://httpbin.org/anything"
        let tokens = alexScanTokens input
        let ast = parse tokens
        print ast

  , testCase "" $ do
        let input = "HTTP 201"
        let tokens = alexScanTokens input
        let ast = parse tokens
        print ast

  , testCase "callable" $ do
        let input = "get http://httpbin.org/anything HTTP 201"
        let tokens = alexScanTokens input
        let ast = parse tokens
        print ast

    -- testGroup "main loop"

  , testCase "interpret statements terminates" $ do
        interpret []

    ]
