{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

import Data.Maybe (fromJust)
import qualified Control.Monad (mapM, mapM_)
import Test.Tasty
import Test.Tasty.HUnit
import L
import P
import Hh200.Etf
import qualified Hh200.Etf as Etf

-- import qualified P as ParserTypes (Statement(..))

toStatements :: String -> Maybe [P.Statement]
toStatements content = do
    let tokensOrPanic = alexScanTokens content
    let tokens = tokensOrPanic

    case parse tokens of
        ParseOk a -> Just a
        _ -> Nothing

doAssertParse :: String -> IO ()
doAssertParse content = do
    let tokensOrPanic = alexScanTokens content
    let tokens = tokensOrPanic

    case parse tokens of
        ParseFailed msg -> assertBool msg False
        _ -> assertBool "" True

-- asTupleTerm :: P.Statement -> IO Etf.Term  -- ??: dynamic (don't return Maybe) with logging
asTupleTerm :: P.Statement -> Etf.Term
asTupleTerm s =
    case s of
        P.Request { method, url } -> TupleTerm [AtomTerm method, asBinaryTerm url]
        -- ??: support http versions erlang side
        P.Response { version, status } -> TupleTerm [AtomTerm "HTTP", IntegerTerm status]

asListTerm :: [P.Statement] -> Etf.Term
asListTerm stmts = ListTerm $ map asTupleTerm stmts

main = defaultMain t

t :: TestTree
t = testGroup "syntax" [

    testCase "parse string" $ do
        let caseReqLine =   ["GET https://example.com\n", "GET https://example.com"]
        let caseVerStatus = ["HTTP/1.1 200", "HTTP/1 200", "HTTP 200"]

        mapM_ doAssertParse (caseReqLine ++ caseVerStatus)

  , testCase "parse multiline string" $ do
        let caseReqLine =   "GET https://example.com\nHTTP 200"
        let stmts = [
                P.Request { method = "GET", url = "https://example.com" },
                P.Response { version = Nothing, status = 200 }
                ] :: [P.Statement]

        assertBool "data struct equality" (toStatements caseReqLine == Just stmts)

  , testCase "parse and write etf" $ do
        let caseReqLine =   "GET http://httpbin.org/get\nHTTP 200"
        let parsed = fromJust $ toStatements caseReqLine :: [Statement]
        let t = asListTerm parsed
        Etf.writeTermToFile "unittest.etf" t

  -- , testCase "parse hhs" $ do
  --       content <- readFile "/home/tbmreza/gh/hh200/examples/hello.hhs"
  --       let tokensOrPanic = alexScanTokens content
  --       let tokens = tokensOrPanic
  --       print tokens
  --
  --       case parse tokens of
  --           -- ParseOk a ->       Etf.writeTermToFile o $ asListTerm a
  --           ParseOk a ->       print "parse ok!"
  --           ParseFailed msg -> print msg


  -- , testCase "" $ do
  --       let input = "POST https://example.com"
  --       let tokens = alexScanTokens input
  --       let ast = parse tokens
  --       print ast
  --
  -- , testCase "" $ do
  --       -- let input = "POST http://localhost:9999/user/12"
  --       let input = "get http://httpbin.org/anything"
  --       let tokens = alexScanTokens input
  --       let ast = parse tokens
  --       print ast
  --
  -- , testCase "" $ do
  --       let input = "HTTP 201"
  --       let tokens = alexScanTokens input
  --       let ast = parse tokens
  --       print ast
  --
  -- , testCase "callable" $ do
  --       let input = "get http://httpbin.org/anything HTTP 201"
  --       let tokens = alexScanTokens input
  --       let ast = parse tokens
  --       print ast

    -- testGroup "main loop"

    --

    -- testCase "interpret statements terminates" $ do
    --     interpret []
    --     interpret [Response (IntLit 201)]
    --     interpret [RequestLine "get" (Url "http" "localhost:9999" [] Nothing Nothing)]
    --     interpret [RequestLine "get" (Url "http" "localhost:9999" [] Nothing Nothing), Response (IntLit 201)]
    --
    --     interpret [RequestLine "get" (Url "http" "bivi-backend.pti-cosmetics.com" [] Nothing Nothing)]

    ]
