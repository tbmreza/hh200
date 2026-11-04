{-# LANGUAGE OverloadedStrings #-}

-- | A small, line-oriented parser for curl's pseudo-XML test files.
--
-- Only the sections hh200 cares about are extracted; everything else is
-- ignored. Each tag sits on its own line, so a prefix scanner is sufficient.
module CurlTest.Parse
    ( parseCurlTest
    , sectionBody
    , firstSection
    , numberedSections
    , isOpenTag
    , isCloseTag
    , attrsOf
    , trim
    ) where

import           Data.Char (isSpace)
import           Data.List (isPrefixOf)
import           Data.Maybe (fromMaybe)

import           CurlTest.Types

-- | Parse one vendored test file. @num@ is the test number (from the filename).
parseCurlTest :: Int -> String -> Either String CurlTest
parseCurlTest num raw = do
    let ls = cleanLines (lines raw)
    tc <- note "missing <testcase>" (sectionBody "testcase" ls)
    let info     = fromMaybe [] (sectionBody "info" tc)
        replyLs  = fromMaybe [] (sectionBody "reply" tc)
        client   = fromMaybe [] (sectionBody "client" tc)
        verifyLs = fromMaybe [] (sectionBody "verify" tc)
    keywords <- note "missing <keywords>" (fmap (map trim) (sectionBody "keywords" info))
    server   <- note "missing <server>"  (fmap (unwords . map trim) (sectionBody "server" client))
    let features = fromMaybe [] (fmap (concatMap words) (sectionBody "features" client))
        name     = fromMaybe "" (fmap (unlines . map trim) (sectionBody "name" client))
    command <- note "missing <command>" (fmap unlines (sectionBody "command" client))
    let stdin      = fmap unlines (sectionBody "stdin" client)
        replyD     = firstSection "data" replyLs
        replyP     = numberedSections "data" replyLs
        protocol   = firstSection "protocol" verifyLs
        errorcode  = parseErrorcodes verifyLs
        stdout     = fmap unlines (sectionBody "stdout" verifyLs)
        stderr     = fmap unlines (sectionBody "stderr" verifyLs)
        strip      = fromMaybe [] (sectionBody "strip" verifyLs)
    pure CurlTest
        { ctNumber   = num
        , ctKeywords = keywords
        , ctName     = name
        , ctServer   = server
        , ctFeatures = features
        , ctCommand  = command
        , ctStdin    = stdin
        , ctReply    = Reply replyD replyP
        , ctVerify   = Verify protocol errorcode stdout stderr strip
        }

note :: String -> Maybe a -> Either String a
note msg = maybe (Left msg) Right

-- | Return the lines between @<name ...>@ and its first matching @</name>@.
sectionBody :: String -> [String] -> Maybe [String]
sectionBody name ls = do
    (o:_) <- Just [i | (i, l) <- zip [0 ..] ls, isOpenTag name l]
    let after = drop (o + 1) ls
    (c:_) <- Just [i | (i, l) <- zip [0 ..] after, isCloseTag name l]
    Just (take c after)

-- | The first section whose tag is exactly @name@ (no numeric suffix).
firstSection :: String -> [String] -> Maybe Section
firstSection name ls = do
    (o:_) <- Just [i | (i, l) <- zip [0 ..] ls, isOpenTag name l]
    let (_n, attrs) = parseOpenTag (trim (ls !! o))
    body <- sectionBody name ls
    Just (Section name attrs (unlines body))

-- | All sections whose tag is @name@ followed by a number (e.g. @data2@).
numberedSections :: String -> [String] -> [Section]
numberedSections name ls =
    [ mk i suffix
    | (i, suffix) <- [ (i, suffix)
                     | (i, l) <- zip [0 ..] ls
                     , Just suffix <- [numberedSuffix name l]
                     ]
    ]
  where
    mk i suffix =
        let (tag, attrs) = parseOpenTag (trim (ls !! i))
            after = drop (i + 1) ls
            closeTag = "</" ++ name ++ suffix ++ ">"
            body = case [j | (j, l) <- zip [0 ..] after, trim l == closeTag] of
                     (c:_) -> take c after
                     []    -> []
        in Section tag attrs (unlines body)

-- | @Just "2"@ if @l@ opens @<name2>@, else @Nothing@.
numberedSuffix :: String -> String -> Maybe String
numberedSuffix name l =
    let t = trim l
        prefix = "<" ++ name
    in if prefix `isPrefixOf` t
          then
              let rest = drop (length prefix) t
              in case rest of
                   (c:_) | c `elem` ['0' .. '9'] ->
                       Just (takeWhile (`elem` ['0' .. '9']) rest)
                   _ -> Nothing
          else Nothing

-- | Does @l@ open a tag named exactly @name@ (attributes allowed)?
isOpenTag :: String -> String -> Bool
isOpenTag name l =
    let t = trim l
        prefix = "<" ++ name
    in prefix `isPrefixOf` t
        && (length t == length prefix
            || let c = t !! length prefix in c == '>' || isSpace c)

isCloseTag :: String -> String -> Bool
isCloseTag name l = trim l == "</" ++ name ++ ">"

-- | Extract @key="value"@ attributes from an open-tag string.
parseOpenTag :: String -> (String, [(String, String)])
parseOpenTag t =
    let body = dropWhile (/= '<') t
        inner = takeWhile (/= '>') (drop 1 body)
        (nm, rest) = break isSpace inner
    in (nm, attrsOf rest)

attrsOf :: String -> [(String, String)]
attrsOf s = go (trim s)
  where
    go t =
        let (k, rest) = break (== '=') t
        in case rest of
             [] -> []
             (_:afterEq) ->
                 let k' = trim k
                 in case afterEq of
                      ('"':afterQ) ->
                          let (v, tail') = break (== '"') afterQ
                          in (k', v) : go (drop 1 tail')
                      _ -> go afterEq

-- | Parse the @<errorcode>@ section (one code per line, comma-separated).
parseErrorcodes :: [String] -> [Int]
parseErrorcodes ls =
    case sectionBody "errorcode" ls of
        Nothing -> []
        Just body -> concatMap parseLine body
  where
    parseLine line =
        [ read tok
        | tok <- splitOn "," (filter (not . isSpace) line)
        , not (null tok)
        ]

-- | Drop XML/shell comment lines. Blank lines are kept: they are semantic
-- (e.g. the header/body separator inside a @<data>@ section).
cleanLines :: [String] -> [String]
cleanLines = filter (not . isComment)

isComment :: String -> Bool
isComment l =
    let t = trim l
    in "#" `isPrefixOf` t || "<!--" `isPrefixOf` t

-- ---------------------------------------------------------------------------
-- String helpers (avoid pulling in extra deps).
-- ---------------------------------------------------------------------------

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn _ [] = error "splitOn: empty delimiter"
splitOn delim xs = go xs
  where
    go s
        | null s = []
        | delim `isPrefixOf` s = [] : go (drop (length delim) s)
        | otherwise =
            let (a, b) = breakOnString delim s
            in a : if null b then [] else go (drop (length delim) b)

breakOnString :: Eq a => [a] -> [a] -> ([a], [a])
breakOnString needle hay = go hay
  where
    go s
        | needle `isPrefixOf` s = ([], s)
        | otherwise =
            case s of
                [] -> ([], [])
                (c:cs) -> let (a, b) = go cs in (c : a, b)
