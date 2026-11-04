{-# LANGUAGE OverloadedStrings #-}

-- | Macro substitution and line-ending handling for curl test data.
--
-- Implements the small subset of curl's preprocessor that the vendored tests
-- actually use: @%TESTNUMBER@, @%HOSTIP@, @%HTTPPORT@, @%VERSION@, the special
-- character macros (@%CR@/@%SP@/@%TAB@/@%AMP@/@%GT@/@%LT@), and the @crlf@
-- attribute on @<data>@/@<protocol>@ sections.
module CurlTest.Preprocess
    ( substitute
    , applyCrlf
    , renderReply
    , replyStatus
    , replyHasSwsclose
    ) where

import           Data.Char (isDigit)
import           Data.List (isInfixOf, isPrefixOf)
import qualified Data.ByteString.Char8 as C8

import           CurlTest.Types

-- | Replace the documented macros. Unrecognised macros are left untouched.
substitute :: Context -> String -> String
substitute ctx = replaceAll
    [ ("%TESTNUMBER", show (ctxNumber ctx))
    , ("%HOSTIP", ctxHost ctx)
    , ("%HTTPPORT", show (ctxPort ctx))
    , ("%VERSION", "0.0.1")
    , ("%CR", "\r")
    , ("%SP", " ")
    , ("%TAB", "\t")
    , ("%AMP", "&")
    , ("%GT", ">")
    , ("%LT", "<")
    ]

replaceAll :: [(String, String)] -> String -> String
replaceAll pairs s0 = foldl (\s (from, to) -> replace from to s) s0 pairs

replace :: String -> String -> String -> String
replace _ _ [] = []
replace from to s@(c:cs)
    | from `isPrefixOf` s = to ++ replace from to (drop (length from) s)
    | otherwise = c : replace from to cs

-- | Apply a @crlf@ attribute value to newlines.
applyCrlf :: Maybe String -> String -> String
applyCrlf Nothing s          = s
applyCrlf (Just "yes") s     = crlfAll s
applyCrlf (Just "headers") s = crlfHeaders s
applyCrlf _ s                = s

-- | Convert every LF to CRLF.
crlfAll :: String -> String
crlfAll = concatMap (\c -> if c == '\n' then "\r\n" else [c])

-- | Convert only the header block (up to the first blank line) to CRLF.
crlfHeaders :: String -> String
crlfHeaders s =
    let (hdr, rest) = breakOnString "\n\n" s
    in if null rest
          then crlfAll s
          else crlfAll hdr ++ "\r\n\r\n" ++ drop 2 rest

-- | Render a reply section to the exact bytes the server should send back.
renderReply :: Context -> Section -> C8.ByteString
renderReply ctx sec =
    C8.pack (applyCrlf crlf (substitute ctx (secBody sec)))
  where
    crlf = lookup "crlf" (secAttrs sec)

-- | The HTTP status code from the first line of a reply body, if any.
replyStatus :: Section -> Maybe Int
replyStatus sec =
    case lines (secBody sec) of
        (firstLine:_) ->
            case words firstLine of
                (_ver : codeStr : _) | all isDigit codeStr -> Just (read codeStr)
                _ -> Nothing
        [] -> Nothing

-- | Does the reply body contain the @swsclose@ keyword (close after send)?
replyHasSwsclose :: Section -> Bool
replyHasSwsclose sec = "swsclose" `isInfixOf` secBody sec

breakOnString :: Eq a => [a] -> [a] -> ([a], [a])
breakOnString needle hay = go hay
  where
    go s
        | needle `isPrefixOf` s = ([], s)
        | otherwise =
            case s of
                [] -> ([], [])
                (c:cs) -> let (a, b) = go cs in (c : a, b)
