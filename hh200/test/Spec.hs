{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

-- import qualified Data.Char as Char (toLower)
-- import Data.Maybe (fromJust)
-- import qualified Control.Monad (mapM, mapM_)
import Test.Tasty
import Test.Tasty.HUnit

-- import qualified Data.ByteString.Lazy.Char8 as L8
--
-- import Control.Monad.Reader
import Hh200.Types
import Hh200.Scanner

-- toProgram :: String -> [Callable]
-- toProgram content = do
--     let tokensOrPanic = alexScanTokens content
--     let tokens = tokensOrPanic
--
--     case parse tokens of
--         ParseOk a -> a
--         _ -> []


doAssertParse :: String -> IO ()
doAssertParse content = do
    let tokensOrPanic = alexScanTokens content
    let tokens = tokensOrPanic

    case parse tokens of
        ParseFailed msg -> assertBool msg False
        _ -> assertBool "" True

main :: IO ()
main = defaultMain t

t :: TestTree
t = testGroup "syntax" [
    testCase "parse string" $ do
        let str = "\"log in\" \"auth\" then \"sellout\" \n post http://localhost:9999/p \n HTTP [200 201] (\"home/pat.jpg\" overwrite)"
        mapM_ doAssertParse [str]

  , testCase "http client" $ do
        runHttpM $ compile "hello.hhs"
        -- return ()

  -- , testCase "http client: host offline" $ do  -- ??: offline host is a different class of leads

  -- -- ??: assert FAIL with message
  -- , testCase "parse # in url fragments and comments" $ do
  --       let str = "http://wikipedia.org/wiki/Google_Chrome#okk\n section#commented"
  --       mapM_ doAssertParse [str]

--     testCase "parse string" $ do
--         -- let caseReqLine =   ["GET https://example.com\n", "GET https://example.com"]
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
--         assertBool "data struct equality" (toProgram str == Just stmts)
--         -- mapM_ doAssertParse [str]
--
--   , testCase "parse multiline string" $ do
--         let caseReqLine = "GET https://example.com\nHTTP 200"
--         let stmts = [
--                 P.Request { method = "GET", url = "https://example.com" },
--                 P.Response { version = Nothing, status = [200] }
--                 ] :: [P.Statement]
--
--         assertBool "data struct equality" (toProgram caseReqLine == Just stmts)
--
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
--         assertBool "data struct equality" (toProgram sect == Just c)
--
--   , testCase "response+section" $ do
--         let c = [
--                 P.Response { version = Nothing, status = [200] }
--               , P.Config { output = "/home/tbmreza/test.jpg", outputExists = "overwrite" }
--                 ]
--         let sect = "HTTP 200\n[Config]\noutput: /home/tbmreza/test.jpg\noutput-exists: overwrite"
--         assertBool "data struct equality" (toProgram sect == Just c)
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
--         assertBool "data struct equality" (toProgram str == Just c)
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
