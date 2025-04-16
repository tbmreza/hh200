{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

import qualified Data.Char as Char (toLower)
import Data.Maybe (fromJust)
import qualified Control.Monad (mapM, mapM_)
import Test.Tasty
import Test.Tasty.HUnit
import L
import P
import Hh200.Etf
import qualified Hh200.Etf as Etf

-- toStatements :: String -> Maybe [P.Statement]
toStatements content = do
    let tokensOrPanic = alexScanTokens content
    let tokens = tokensOrPanic

    case parse tokens of
        ParseOk a -> Just a
        _ -> Nothing

data Cble = Cble { deps :: [String] }

toCallables :: String -> Maybe [Cble]
toCallables _ = Nothing

doAssertParse :: String -> IO ()
doAssertParse content = do
    let tokensOrPanic = alexScanTokens content
    let tokens = tokensOrPanic

    case parse tokens of
        ParseFailed msg -> assertBool msg False
        _ -> assertBool "" True

methodAtom :: String -> Etf.Term
methodAtom m = AtomTerm $ (map Char.toLower) m

-- -- asTupleTerm :: P.Statement -> IO Etf.Term  -- ??: dynamic (don't return Maybe) with logging
-- asTupleTerm :: P.Statement -> Etf.Term
-- asTupleTerm s =
--     case s of
--         P.Request { method, url } -> TupleTerm [methodAtom method, asBinaryTerm url]
--         -- P.Response { version, status } -> TupleTerm [AtomTerm "http", IntegerTerm status]  -- ??
--         P.Response { version, status } -> TupleTerm [AtomTerm "http"]

-- asListTerm :: [P.Statement] -> Etf.Term
-- asListTerm stmts = ListTerm $ map asTupleTerm stmts

parsed :: Etf.Term
parsed =
    -- ListTerm [
    --     TupleTerm [AtomTerm "post", asBinaryTerm "http://localhost:9999/413-Content-Too-Large.php"]
    --   , TupleTerm [AtomTerm "json", TupleTerm [AtomTerm "mut", asBinaryTerm "/home/tbmreza/gh/hh200/building-blocks/rt/asset.json", IntegerTerm 9]]
    --   , TupleTerm [AtomTerm "probe_valid_size", IntegerTerm 4100, IntegerTerm 200]
    --   ]

    --   {deps, []}
    -- ListTerm [
    --     TupleTerm [AtomTerm "deps", ListTerm []]
    --   ]

    -- T = [{{deps, []},
    --       "login",
    --       {req, post, <<"http://localhost:9999/p">>, [], <<>>, []},
    --       {resp, 200, {output, <<"outfile">>}},
    --       {been_called, false},
    --       {err_stack, []}}],

    ListTerm [
        TupleTerm [
            TupleTerm [AtomTerm "deps", ListTerm []],
            asBinaryTerm "login"],
            TupleTerm [AtomTerm "req", AtomTerm "post", asBinaryTerm "http://localhost:9999/p", ListTerm [], asBinaryTerm "", ListTerm []],
            TupleTerm [AtomTerm "resp", IntegerTerm 200, TupleTerm [AtomTerm "output", asBinaryTerm "outfile"]],
            TupleTerm [AtomTerm "been_called", AtomTerm "false"],
            TupleTerm [AtomTerm "err_stack", ListTerm []]
      ]

main = defaultMain t

t :: TestTree
t = testGroup "syntax" [
    testCase "parse string" $ do
        let str = "\"log in\" \"auth\" then \"sellout\" \n post http://localhost:9999/p"
        -- let str = "\"log in\" then"
        -- let stmts = []
        -- assertBool "data struct equality" (toStatements str == Just stmts)
        mapM_ doAssertParse [str]


--     testCase "parse string" $ do
--         -- let caseReqLine =   ["GET https://example.com\n", "GET https://example.com"]
--         -- ??: revive version sep
--         -- let caseVerStatus = ["HTTP/1.1 200", "HTTP/1 200", "HTTP 200"]
--         -- let caseVerStatus = ["HTTP/1 200", "HTTP 200"]
--
--         -- mapM_ doAssertParse (caseReqLine ++ caseVerStatus)
--         mapM_ doAssertParse ["GET https://example.com\n", "GET https://example.com"]
--
--   , testCase "parse url" $ do
--         let str = "GET https://fastly.picsum.photos/id/19/200/200.jpg?hmac=U8dBrPCcPP89QG1EanVOKG3qBsZwAvtCLUrfeXdE0FI"
--         let stmts = [
--                 P.Request { method = "GET", url = "https://fastly.picsum.photos/id/19/200/200.jpg?hmac=U8dBrPCcPP89QG1EanVOKG3qBsZwAvtCLUrfeXdE0FI" }
--                 ] :: [P.Statement]
--
--         assertBool "data struct equality" (toStatements str == Just stmts)
--         -- mapM_ doAssertParse [str]
--
--   , testCase "parse multiline string" $ do
--         let caseReqLine = "GET https://example.com\nHTTP 200"
--         let stmts = [
--                 P.Request { method = "GET", url = "https://example.com" },
--                 P.Response { version = Nothing, status = [200] }
--                 ] :: [P.Statement]
--
--         assertBool "data struct equality" (toStatements caseReqLine == Just stmts)
--
--
--   , testCase "parse and write etf" $ do
--         let str = "GET http://localhost:9999/p\nHTTP 200"
--         let t = asListTerm (fromJust $ toStatements str)
--         Etf.writeTermToFile "unittest.etf" t
--
--   , testCase "??" $ do
--         let str = "\"login\" then \"checkin\""
--         let t = asListTerm (fromJust $ toStatements str)
--         assertBool "" True
--         -- Etf.writeTermToFile "unittest.etf" t
--
--   -- , testCase "parse section string" $ do
--   --       let caseCapturesSec =   "[Captures]\nnext_id: 12"
--   --       -- let s1 = 
--   --       assertBool "section_name eq" True
--
--   , testCase "status code list" $ do
--         let cases = ["HTTP 200", "HTTP [200]", "HTTP [400 401 500]"]
--         mapM_ doAssertParse cases
--
--   , testCase "section" $ do
--         -- let cases = ["[Options]"]  ??: lexer conflict with options the method
--         let c = [Config { output = "/home/tbmreza/test.jpg", outputExists = "overwrite" }]
--         let sect = "[Config]\noutput: /home/tbmreza/test.jpg\noutput-exists: overwrite"
--
--         assertBool "data struct equality" (toStatements sect == Just c)
--
--   , testCase "response+section" $ do
--         let c = [
--                 P.Response { version = Nothing, status = [200] }
--               , P.Config { output = "/home/tbmreza/test.jpg", outputExists = "overwrite" }
--                 ]
--         let sect = "HTTP 200\n[Config]\noutput: /home/tbmreza/test.jpg\noutput-exists: overwrite"
--         assertBool "data struct equality" (toStatements sect == Just c)
--         -- mapM_ doAssertParse [sect]
--
--   , testCase "request+response+section" $ do
--         let str = "GET https://fastly.picsum.photos/id/19/200/200.jpg?hmac=U8dBrPCcPP89QG1EanVOKG3qBsZwAvtCLUrfeXdE0FI\nHTTP 200\n[Config]\noutput: /home/tbmreza/test.jpg\noutput-exists: overwrite"
--         let c = [
--                 P.Request { method = "GET", url = "https://fastly.picsum.photos/id/19/200/200.jpg?hmac=U8dBrPCcPP89QG1EanVOKG3qBsZwAvtCLUrfeXdE0FI" }
--               , P.Response { version = Nothing, status = [200] }
--               , P.Config { output = "/home/tbmreza/test.jpg", outputExists = "overwrite" }
--                 ]
--
--         assertBool "data struct equality" (toStatements str == Just c)
--
--
-- -- let caseVerStatus = ["HTTP/1.1 200", "HTTP/1 200", "HTTP 200", "HTTP [200]"]
--
--   -- , testCase "parse hhs" $ do
--   --       content <- readFile "/home/tbmreza/gh/hh200/examples/hello.hhs"
--   --       let tokensOrPanic = alexScanTokens content
--   --       let tokens = tokensOrPanic
--   --       print tokens
--   --
--   --       case parse tokens of
--   --           -- ParseOk a ->       Etf.writeTermToFile o $ asListTerm a
--   --           ParseOk a ->       print "parse ok!"
--   --           ParseFailed msg -> print msg
--
--
--   -- , testCase "" $ do
--   --       let input = "POST https://example.com"
--   --       let tokens = alexScanTokens input
--   --       let ast = parse tokens
--   --       print ast
--   --
--   -- , testCase "" $ do
--   --       -- let input = "POST http://localhost:9999/user/12"
--   --       let input = "get http://httpbin.org/anything"
--   --       let tokens = alexScanTokens input
--   --       let ast = parse tokens
--   --       print ast
--   --
--   -- , testCase "" $ do
--   --       let input = "HTTP 201"
--   --       let tokens = alexScanTokens input
--   --       let ast = parse tokens
--   --       print ast
--   --
--   -- , testCase "callable" $ do
--   --       let input = "get http://httpbin.org/anything HTTP 201"
--   --       let tokens = alexScanTokens input
--   --       let ast = parse tokens
--   --       print ast
--
--     -- testGroup "main loop"
--
--     --
--
--     -- testCase "interpret statements terminates" $ do
--     --     interpret []
--     --     interpret [Response (IntLit 201)]
--     --     interpret [RequestLine "get" (Url "http" "localhost:9999" [] Nothing Nothing)]
--     --     interpret [RequestLine "get" (Url "http" "localhost:9999" [] Nothing Nothing), Response (IntLit 201)]
--     --
--     --     interpret [RequestLine "get" (Url "http" "bivi-backend.pti-cosmetics.com" [] Nothing Nothing)]

    ]
