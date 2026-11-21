{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | End-to-end behaviour of 'runScriptM' against a local server: the verdict a
-- script gets for status, header, JSON-body and @[Asserts]@ expectations.
--
-- These pin the observable outcome of the response checks in
-- "Hh200.Execution" so that refactors of that code path (e.g. removing the
-- @Debug.Trace@ calls around 'jsonSubset') cannot silently change verdicts.
module ExecutionRunSpec (spec) where

import           Control.Exception (SomeException, fromException, try)
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Network.HTTP.Client as HC
import           Network.HTTP.Types.Status (status418)
import           Network.Wai.Handler.Warp (testWithApplication)
import           Test.Tasty
import           Test.Tasty.HUnit
import qualified Web.Scotty as S

import           Hh200.Execution (AssertionFailure (..), runScriptM)
import           Hh200.Scanner (analyze)
import           Hh200.Types (Snippet (..), newEnv)

data Outcome = Ok | AssertFail | HttpErr | Other String
    deriving (Show, Eq)

-- | A tiny fixed server the scripts run against.
fixtureApp :: S.ScottyM ()
fixtureApp = do
    S.get "/json" $ do
        S.setHeader "X-Probe" "yes"
        S.setHeader "Content-Type" "application/json"
        S.raw "{\"a\":1,\"b\":{\"c\":2},\"s\":\"x\"}"
    S.get "/teapot" $ do
        S.status status418
        S.text "short and stout"
    S.get "/empty" $ S.raw ""

-- | Run a script (with @PORT@ replaced by the fixture's port) and classify it.
runOn :: String -> IO Outcome
runOn src = do
    app <- S.scottyApp fixtureApp
    testWithApplication (pure app) $ \port -> do
        let rendered = replace "PORT" (show port) src
        parsed <- analyze (Snippet (L8.pack rendered))
        case parsed of
            Left err -> assertFailure ("fixture script does not parse: " ++ err ++ "\n" ++ rendered)
                        >> pure (Other "unparsed")
            Right script -> do
                r <- try (runScriptM script newEnv) :: IO (Either SomeException ())
                pure $ case r of
                    Right () -> Ok
                    Left e
                        | Just (_ :: AssertionFailure) <- fromException e -> AssertFail
                        | Just (_ :: HC.HttpException) <- fromException e -> HttpErr
                        | otherwise -> Other (show e)

replace :: String -> String -> String -> String
replace from to = go
  where
    go [] = []
    go s@(c:cs)
      | take (length from) s == from = to ++ go (drop (length from) s)
      | otherwise = c : go cs

expect :: Outcome -> String -> String -> TestTree
expect want name src = testCase name $ do
    got <- runOn src
    assertEqual ("outcome of:\n" ++ src) want got

spec :: TestTree
spec = testGroup "Execution (end-to-end verdicts)"
  [ testGroup "status"
      [ expect Ok "matching status passes"
          "GET http://127.0.0.1:PORT/json\nHTTP 200\n"
      , expect Ok "default expectation is 200"
          "GET http://127.0.0.1:PORT/json\n"
      , expect AssertFail "mismatched status fails"
          "GET http://127.0.0.1:PORT/json\nHTTP 404\n"
      , expect Ok "non-200 status can be expected"
          "GET http://127.0.0.1:PORT/teapot\nHTTP 418\n"
      , expect AssertFail "unexpected non-200 fails"
          "GET http://127.0.0.1:PORT/teapot\nHTTP 200\n"
      ]
  , testGroup "json body (subset semantics)"
      [ expect Ok "expected body that is a strict subset passes"
          "GET http://127.0.0.1:PORT/json\nHTTP 200\n{\"a\": 1}\n"
      , expect Ok "identical body passes"
          "GET http://127.0.0.1:PORT/json\nHTTP 200\n{\"a\":1,\"b\":{\"c\":2},\"s\":\"x\"}\n"
      , expect Ok "identical body with reordered keys and whitespace passes"
          "GET http://127.0.0.1:PORT/json\nHTTP 200\n{ \"s\": \"x\", \"b\": { \"c\": 2 }, \"a\": 1 }\n"
      , expect Ok "nested subset passes"
          "GET http://127.0.0.1:PORT/json\nHTTP 200\n{\"b\": {\"c\": 2}}\n"
      , expect AssertFail "different value fails"
          "GET http://127.0.0.1:PORT/json\nHTTP 200\n{\"a\": 2}\n"
      , expect AssertFail "key absent from response fails"
          "GET http://127.0.0.1:PORT/json\nHTTP 200\n{\"a\": 1, \"z\": 0}\n"
      , expect AssertFail "json expectation against a non-json body fails"
          "GET http://127.0.0.1:PORT/teapot\nHTTP 418\n{\"a\": 1}\n"
      , expect Ok "no body expectation skips the body check"
          "GET http://127.0.0.1:PORT/empty\nHTTP 200\n"
      ]
  , testGroup "headers"
      [ expect Ok "present response header passes"
          "GET http://127.0.0.1:PORT/json\nHTTP 200\nX-Probe: \"yes\"\n"
      , expect AssertFail "wrong response header value fails"
          "GET http://127.0.0.1:PORT/json\nHTTP 200\nX-Probe: \"no\"\n"
      ]
  , testGroup "transport"
      [ expect HttpErr "connection refused is an HTTP error, not an assertion failure"
          "GET http://127.0.0.1:1/json\nHTTP 200\n"
      ]
  ]
