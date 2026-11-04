{-# LANGUAGE OverloadedStrings #-}

module CurlSpec where

import qualified Data.Text as T
import           System.Directory (doesFileExist)
import           Test.Tasty
import           Test.Tasty.HUnit

import           Hh200.CurlTest

spec :: TestTree
spec = testGroup "curl"
    [ testCase ("curl/test" ++ number ++ " " ++ keywords) $ do
        parsed <- readCorpus path
        case parsed of
            Left err -> assertFailure err
            Right test -> assertParsed test
    | (path, number, keywords) <- corpus
    ]

corpus :: [(FilePath, String, String)]
corpus =
    [ ("test/curl/data/test207", "207", "HTTP HTTP GET")
    , ("test/curl/data/test214", "214", "HTTP HTTP GET")
    ]

readCorpus :: FilePath -> IO (Either String CurlTest)
readCorpus path = do
    inRoot <- doesFileExist path
    readCurlTest (if inRoot then path else "../" ++ path)

assertParsed :: CurlTest -> Assertion
assertParsed test = do
    assertBool "HTTP URL" ("http" `T.isPrefixOf` curlUrl (curlRequest test))
    assertBool "method is present" (not (T.null (curlMethod (curlRequest test))))
    assertBool "response fixture is present" (curlReply test /= Nothing)
    assertBool "request expectation is present" (curlExpectedProtocol test /= Nothing)
    case toHhScript test of
        Left err -> assertFailure (show err)
        Right script -> assertBool "translated script is non-empty" (not (T.null script))
