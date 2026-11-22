{-# LANGUAGE OverloadedStrings #-}

-- | Translate a curl @<command>@ into an hh200 request and the outcome hh200
-- is expected to observe when the replayed @<reply>@ comes back.
--
-- Only the supported subset is handled: URL, method (@-X@ / @--request@ /
-- @-T -@ / @-d@), and body (@-d@/ @--data@ / @--data-binary@ / @<stdin>@).
-- Anything else must have been excluded by the skip list before we get here.
module CurlTest.Translate
    ( translate
    , tokenize
    , urlToTarget
    , detectMethod
    , detectBody
    ) where

import           Data.Char (isSpace)
import           Data.List (isInfixOf)
import           Data.Maybe (listToMaybe)
import qualified Data.ByteString.Char8 as C8

import qualified Hh200.Curl as Curl

import           CurlTest.Types
import           CurlTest.Preprocess (substitute, replyStatus)
import           CurlTest.Util (breakOnString)

-- | Derive the request plan and expected outcome for a test.
translate :: Context -> CurlTest -> Either String (RequestPlan, Expected)
translate ctx ct = do
    urlTok <- note "no URL in <command>" (findUrl (ctCommand ct))
    let target = urlToTarget (substitute ctx urlTok)
        method = detectMethod (ctCommand ct)
        body   = detectBody ct
    pure (RequestPlan method target body, detectExpected ct)
  where
    note msg = maybe (Left msg) Right

-- | The expected outcome, derived from the reply data and @<errorcode>@.
detectExpected :: CurlTest -> Expected
detectExpected ct =
    case replyData (ctReply ct) of
        Nothing -> ExpectHttpError
        Just sec
            | empty (secBody sec) || any (`elem` [7, 52]) (vErrorcode (ctVerify ct)) ->
                ExpectHttpError
            | maybe False (>= 400) (replyStatus sec) || 22 `elem` vErrorcode (ctVerify ct) ->
                ExpectAssertFail
            | otherwise -> ExpectOk
  where
    empty s = all isSpace s

-- | The request-target (path?query) of a URL, with an empty path as @/@.
urlToTarget :: String -> String
urlToTarget url =
    let afterScheme = drop 3 (snd (breakOnString "://" url))
        (_, rest) = break (\c -> c == '/' || c == '?' || c == '#') afterScheme
    in case rest of
         []         -> "/"
         ('?':_)    -> '/' : rest
         _          -> rest

-- | Shared with the library's curl front end ('Curl.curlMethod').
detectMethod :: String -> String
detectMethod = Curl.curlMethod . tokenize

detectBody :: CurlTest -> C8.ByteString
detectBody ct =
    case ctStdin ct of
        Just s -> C8.pack s
        Nothing ->
            case dataFlag (ctCommand ct) of
                Just b -> C8.pack b
                Nothing -> C8.pack ""

dataFlag :: String -> Maybe String
dataFlag = Curl.curlDataArg . tokenize

findUrl :: String -> Maybe String
findUrl cmd =
    listToMaybe [ t | t <- tokenize cmd, "://" `isInfixOf` t ]

-- | Split a command line into shell words (the library's 'Curl.shellWords').
tokenize :: String -> [String]
tokenize = Curl.shellWords
