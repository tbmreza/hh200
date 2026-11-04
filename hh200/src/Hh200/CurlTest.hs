{-# LANGUAGE OverloadedStrings #-}

module Hh200.CurlTest
    ( CurlTest (..)
    , CurlRequest (..)
    , CurlResponse (..)
    , CurlParseError (..)
    , parseCurlTest
    , readCurlTest
    , toHhScript
    ) where

import           Control.Exception (IOException, try)
import qualified Data.ByteString.Lazy as LBS
import           Data.Char (isSpace, toUpper)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           System.FilePath (takeFileName)

data CurlTest = CurlTest
    { curlTestNumber :: Int
    , curlKeywords :: [T.Text]
    , curlName :: T.Text
    , curlRequest :: CurlRequest
    , curlReply :: Maybe CurlResponse
    , curlExpectedProtocol :: Maybe LBS.ByteString
    } deriving (Eq, Show)

data CurlRequest = CurlRequest
    { curlMethod :: T.Text
    , curlUrl :: T.Text
    , curlHeaders :: [(T.Text, T.Text)]
    , curlBody :: LBS.ByteString
    } deriving (Eq, Show)

data CurlResponse = CurlResponse
    { curlResponseBytes :: LBS.ByteString
    , curlResponseStatus :: Maybe Int
    } deriving (Eq, Show)

data CurlParseError
    = MissingTag T.Text
    | UnsupportedFeature T.Text
    | InvalidValue T.Text
    | InvalidCommand T.Text
    deriving (Eq, Show)

readCurlTest :: FilePath -> IO (Either String CurlTest)
readCurlTest path = do
    result <- try (readFile path) :: IO (Either IOException String)
    pure $ case result of
        Left err -> Left (show err)
        Right source ->
            case parseCurlTest path (T.pack source) of
                Left err -> Left (show err)
                Right test -> Right test

parseCurlTest :: FilePath -> T.Text -> Either CurlParseError CurlTest
parseCurlTest path source = do
    let number = readNumber path
    info <- required "info" source
    client <- required "client" source
    let keywords = linesIn <$> optional "keywords" info
        name = maybe (T.pack ("curl test " ++ show number)) firstLine (optional "name" client)
        command = maybe "" firstLine (optional "command" client)
    request <- parseCommand command
    protocol <- optional "protocol" (maybe "" id (optional "verify" source))
    reply <- parseReply (maybe "" id (optional "reply" source))
    pure CurlTest
        { curlTestNumber = number
        , curlKeywords = keywords
        , curlName = name
        , curlRequest = request
        , curlReply = reply
        , curlExpectedProtocol = TE.encodeUtf8 . T.pack <$> protocol
        }

parseReply :: T.Text -> Either CurlParseError (Maybe CurlResponse)
parseReply reply
    | T.null (T.strip reply) = Right Nothing
    | otherwise =
        case optional "data" reply of
            Nothing -> Right Nothing
            Just bytes ->
                let first = firstLine bytes
                    status = case T.words first of
                        ("HTTP/1.0":code:_) -> readInt code
                        ("HTTP/1.1":code:_) -> readInt code
                        _ -> Nothing
                in Right (Just (CurlResponse (TE.encodeUtf8 bytes) status))

parseCommand :: T.Text -> Either CurlParseError CurlRequest
parseCommand command =
    let tokens = shellWords (T.unpack (T.strip command))
        (method, rest) = extractMethod tokens
        (headers, body, remaining) = extractOptions rest [] Nothing
    in case remaining of
        [url] | "http://" `T.isPrefixOf` T.pack url || "https://" `T.isPrefixOf` T.pack url ->
            Right CurlRequest
                { curlMethod = T.pack (if body /= Nothing && method == "GET" then "POST" else method)
                , curlUrl = T.pack (unescapeUrl url)
                , curlHeaders = headers
                , curlBody = maybe "" (LBS.fromStrict . TE.encodeUtf8 . T.pack) body
                }
        _ -> Left (InvalidCommand command)
  where
    extractMethod ("-X":m:xs) = (map toUpper m, xs)
    extractMethod ("--request":m:xs) = (map toUpper m, xs)
    extractMethod xs = ("GET", xs)

    extractOptions ("-H":h:xs) headers body = addHeader h xs headers body
    extractOptions ("--header":h:xs) headers body = addHeader h xs headers body
    extractOptions ("--data":value:xs) headers _ = extractOptions xs headers (Just value)
    extractOptions ("--data-raw":value:xs) headers _ = extractOptions xs headers (Just value)
    extractOptions ("--data-binary":value:xs) headers _ = extractOptions xs headers (Just value)
    extractOptions ("--data-urlencode":value:xs) headers _ = extractOptions xs headers (Just value)
    extractOptions xs headers body = (headers, body, xs)

    addHeader header xs headers body =
        case break (== ':') header of
            (key, ':':value) -> extractOptions xs ((T.pack key, T.strip (T.pack value)):headers) body
            _ -> (headers, body, header:xs)

toHhScript :: CurlTest -> Either CurlParseError T.Text
toHhScript test =
    let request = curlRequest test
        response = curlReply test
        headerLines = map (\(key, value) -> key <> ": " <> value) (curlHeaders request)
        requestLines = [curlMethod request <> " " <> curlUrl request] ++ headerLines
            ++ if LBS.null (curlBody request)
                then []
                else ["", TE.decodeUtf8With TE.lenientDecode (LBS.toStrict (curlBody request))]
        responseLines = case response of
            Nothing -> []
            Just value ->
                maybe [] (\code -> ["HTTP " <> T.pack (show code)]) (curlResponseStatus value)
                ++ responseHeadersAndBody (curlResponseBytes value)
    in Right (T.unlines (requestLines ++ [""] ++ responseLines))

responseHeadersAndBody :: LBS.ByteString -> [T.Text]
responseHeadersAndBody bytes =
    let text = TE.decodeUtf8With TE.lenientDecode (LBS.toStrict bytes)
        withoutStatus = case T.breakOn "\n" text of
            (_, rest) -> T.drop 1 rest
        (headers, body) = T.breakOn "\n\n" withoutStatus
        headerLines = filter (not . T.null) (T.lines headers)
    in if T.null body then headerLines else headerLines ++ ["", T.drop 2 body]

required :: T.Text -> T.Text -> Either CurlParseError T.Text
required tag source = maybe (Left (MissingTag tag)) Right (optional tag source)

optional :: T.Text -> T.Text -> Maybe T.Text
optional tag source = do
    let close = "</" <> tag <> ">"
        candidates = filter (not . T.null . snd)
            [T.breakOn ("<" <> tag <> ">") source
            ,T.breakOn ("<" <> tag <> " ") source]
        (_, afterOpen) = minimumByOffset candidates
        afterTag = T.drop 1 (T.dropWhile (/= '>') afterOpen)
        (body, _) = T.breakOn close afterTag
    if T.null afterOpen || T.null body then Nothing else Just (stripXmlComments body)
  where
    minimumByOffset [] = ("", "")
    minimumByOffset values = foldr1 earlier values
    earlier left@(_, a) right@(_, b)
        | T.length a <= T.length b = left
        | otherwise = right

stripXmlComments :: T.Text -> T.Text
stripXmlComments = T.unlines . filter (not . isComment) . T.lines
  where
    isComment line = "<!--" `T.isPrefixOf` T.strip line || "#" `T.isPrefixOf` T.strip line

linesIn :: T.Text -> [T.Text]
linesIn = filter (not . T.null) . map T.strip . T.lines

firstLine :: T.Text -> T.Text
firstLine = T.strip . headOr ""
  . filter (not . T.null)
  . map T.strip
  . T.lines

headOr :: a -> [a] -> a
headOr fallback [] = fallback
headOr _ (x:_) = x

readInt :: T.Text -> Maybe Int
readInt value =
    case reads (T.unpack value) of
        [(number, "")] -> Just number
        _ -> Nothing

readNumber :: FilePath -> Int
readNumber path =
    let filename = takeFileName path
        digits = takeWhile (`elem` ['0'..'9']) (dropWhile (not . (`elem` ['0'..'9'])) filename)
    in case reads digits of
        [(number, "")] -> number
        _ -> 0

unescapeUrl :: String -> String
unescapeUrl [] = []
unescapeUrl ('\\':x:xs) = x : unescapeUrl xs
unescapeUrl (x:xs) = x : unescapeUrl xs

shellWords :: String -> [String]
shellWords = go [] [] Nothing
  where
    go words current quote [] =
        reverse (if null current then words else reverse current : words)
    go words current quote (x:xs)
        | Just q <- quote, x == q = go words current Nothing xs
        | quote == Nothing && (x == '"' || x == '\'') = go words current (Just x) xs
        | quote == Nothing && isSpace x =
            go (if null current then words else reverse current : words) [] Nothing xs
        | otherwise = go words (x:current) quote xs
