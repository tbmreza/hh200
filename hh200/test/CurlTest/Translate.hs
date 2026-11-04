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
    ) where

import           Data.Char (isSpace)
import           Data.List (isInfixOf)
import           Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.ByteString.Char8 as C8

import           CurlTest.Types
import           CurlTest.Preprocess (substitute, replyStatus)

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

detectMethod :: String -> String
detectMethod cmd =
    let toks = tokenize cmd
        explicit = firstAfter ["-X", "--request"] toks
        hasData  = any (`elem` ["-d", "--data", "--data-binary", "--json"]) toks
        hasPut   = "-T" `elem` toks
        implied  = if hasPut then "PUT" else if hasData then "POST" else "GET"
    in fromMaybe implied explicit

detectBody :: CurlTest -> C8.ByteString
detectBody ct =
    case ctStdin ct of
        Just s -> C8.pack s
        Nothing ->
            case dataFlag (ctCommand ct) of
                Just b -> C8.pack b
                Nothing -> C8.pack ""

dataFlag :: String -> Maybe String
dataFlag cmd =
    let toks = tokenize cmd
        flags = ["-d", "--data", "--data-binary"]
    in firstAfter flags toks

-- | First token after the first occurrence of one of the given flags.
firstAfter :: [String] -> [String] -> Maybe String
firstAfter flags toks =
    listToMaybe [ toks !! (i + 1)
                | (i, t) <- zip [0 ..] toks
                , t `elem` flags
                , i + 1 < length toks
                ]

findUrl :: String -> Maybe String
findUrl cmd =
    listToMaybe [ t | t <- tokenize cmd, "://" `isInfixOf` t ]

-- | Split a command line on whitespace, keeping double-quoted spans intact.
tokenize :: String -> [String]
tokenize = go . dropWhile isSpace
  where
    go [] = []
    go ('"':rest) =
        let (tok, after) = break (== '"') rest
        in tok : go (drop 1 after)
    go t =
        let (tok, after) = break isSpace t
        in tok : go (dropWhile isSpace after)

breakOnString :: Eq a => [a] -> [a] -> ([a], [a])
breakOnString needle hay = go hay
  where
    go s
        | needle `isPrefixOf` s = ([], s)
        | otherwise =
            case s of
                [] -> ([], [])
                (c:cs) -> let (a, b) = go cs in (c : a, b)

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
