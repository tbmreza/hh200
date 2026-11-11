{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Re-export lexer and parser generated code.
module Hh200.Scanner
    ( module Hh200.Scanner
    , module L  -- ??: if test modules can import this directly, rm this re-exports
    , module P
    -- , P.hasBalancedMustache
    ) where

import           System.Directory (doesFileExist)
import           System.Process (readProcess)
import qualified System.Info as Info

import           Control.Monad.Trans.Except (runExceptT)
import           Control.Exception (try, IOException)

import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Char (toLower)
import           Data.List (isInfixOf, isPrefixOf)
import           Data.Text (Text)
import qualified Data.Text as Text

-- import Hh200.Types (Script(..), HostInfo(..), Snippet(..), hiHh200Conf, defaultHostInfo)
import Hh200.Types (Script(..), HostInfo(..), Snippet(..), defaultHostInfo, SrcPos, SrcSpan(..), DocIndex(..), DiagCode(..), Diagnostic(..))
import L
import P


gatherHostInfo :: IO HostInfo
gatherHostInfo = do
    hn <- tryReadProcess "hostname" []
    up <- tryReadProcess "uptime" ["-p"]
    
    pure defaultHostInfo
        { hiHostname = hn
        , hiUptime = up
        , hiOs = Just Info.os
        , hiArch = Just Info.arch
        }

tryReadProcess :: FilePath -> [String] -> IO (Maybe String)
tryReadProcess cmd args = do
    result <- try (readProcess cmd args "") :: IO (Either IOException String)
    case result of
        Left _ -> pure Nothing
        Right s -> pure (Just (trim s))

trim :: String -> String
trim = f . f where
    f = reverse . dropWhile (== ' ') . dropWhile (== '\n')


class Analyze a where
    analyze :: a -> IO (Either String Script)

instance Analyze Snippet where
    analyze :: Snippet -> IO (Either String Script)
    analyze (Snippet s) =
        case scanSafe (L8.unpack s) of
            Left e -> pure (Left (lexErrorMsg e))
            Right tokens -> fmap (either (Left . fst) Right) (parseTokens tokens)

instance Analyze FilePath where
    analyze :: FilePath -> IO (Either String Script)
    analyze path = do
        exists <- doesFileExist path
        case exists of
            False -> pure (Left ("file not found: " ++ path))
            True  -> readScript path

readScript :: FilePath -> IO (Either String Script)
readScript path = do
    loaded <- readFile path
    case scanSafe loaded of
        Left e -> pure (Left (lexErrorMsg e))
        Right tokens -> fmap (either (Left . fst) Right) (parseTokens tokens)

scanSafe :: String -> Either LexError [Token]
scanSafe = alexScanTokens

complete :: String -> Int -> [String]
complete input pos = 
    let prefix = take pos input
        revPrefix = reverse prefix
        partial = reverse $ takeWhile (`notElem` (" \t\n" :: String)) revPrefix

        -- Determine context by scanning the prefix
        tokens = case scanSafe prefix of
            Right ts -> ts
            Left _ -> [] -- Fallback if scan fails

        blockHeaders = filter isBlockHeader tokens
        lastBlock = if null blockHeaders then Nothing else Just (last blockHeaders)

        isBlockHeader (KW_CONFIGS _) = True
        isBlockHeader (KW_CAPTURES _) = True
        isBlockHeader (KW_ASSERTS _) = True
        isBlockHeader _ = False

        candidates = case lastBlock of
            Just (KW_CONFIGS _) -> [ "use-tls" ]
            _ -> [ "GET", "POST", "PUT", "DELETE", "PATCH", "OPTIONS", "HEAD", "TRACE", "CONNECT"
                 , "HTTP"
                 , "Configs", "Captures", "Asserts"
                 , "then"
                 ] ++ extractCaptures tokens

        matches cand = (map toLower partial) `isPrefixOf` (map toLower cand)
    in filter matches candidates

extractCaptures :: [Token] -> [String]
extractCaptures [] = []
extractCaptures (KW_CAPTURES _ : rest) = collectIds rest
    where
    collectIds (IDENTIFIER _ s : ts) = s : collectIds ts
    -- Stop at next block header or EOF, skip other tokens (like COLON, RHS)
    collectIds (t:ts)
        | isBlockStart t = extractCaptures (t:ts)
        | otherwise = collectIds ts
    collectIds [] = []

extractCaptures (_:rest) = extractCaptures rest

getPos :: Token -> AlexPosn
getPos t = case t of
    LN p -> p
    DIGITS p _ -> p
    IDENTIFIER p _ -> p
    SEP p -> p
    METHOD p _ -> p
    VERSION p _ -> p
    BRACE_OPN p -> p
    BRACE_CLS p -> p
    PAREN_OPN p -> p
    PAREN_CLS p -> p
    LIST_OPN p -> p
    LIST_CLS p -> p
    COLON p -> p
    QUOTE p -> p
    KW_THEN p -> p
    KW_HTTP p -> p
    KW_CONFIGS p -> p
    KW_QUERY p -> p
    KW_FORM p -> p
    KW_COOKIES p -> p
    KW_MULTIPART p -> p
    KW_CAPTURES p -> p
    KW_ASSERTS p -> p
    URL p _ -> p
    QUOTED p _ -> p
    BRACED p _ -> p
    BRACE_ENCLOSED p _ -> p
    RHS p _ -> p
    JSONPATH p _ -> p
    LINE p _ -> p
    LEX_ERROR p _ -> p
    EOF p -> p
    _ -> undefined

tokenSpan :: Token -> (Int, Int)
tokenSpan t = (offset, len)
    where
    (AlexPn offset _ _) = getPos t
    len = case t of
        LN _ -> 1
        DIGITS _ s -> length s
        IDENTIFIER _ s -> length s
        SEP _ -> 1
        METHOD _ s -> length s
        VERSION _ s -> length s
        BRACE_OPN _ -> 1
        BRACE_CLS _ -> 1
        PAREN_OPN _ -> 1
        PAREN_CLS _ -> 1
        LIST_OPN _ -> 1
        LIST_CLS _ -> 1
        COLON _ -> 1
        QUOTE _ -> 1
        KW_THEN _ -> 4
        KW_HTTP _ -> 4
        KW_CONFIGS _ -> 7 -- Configs
        KW_CAPTURES _ -> 8 -- Captures
        KW_ASSERTS _ -> 7 -- Asserts
        URL _ s -> length s
        QUOTED _ s -> length s -- includes quotes
        BRACED _ s -> length s -- includes braces
        BRACE_ENCLOSED _ s -> length s
        RHS _ s -> length s -- includes colon
        JSONPATH _ s -> length s
        LINE _ s -> length s -- includes >
        LEX_ERROR _ _ -> 1
        EOF _ -> 0
        _ -> undefined

hover :: String -> Int -> Maybe String
hover input pos = 
    case scanSafe input of
        Left _ -> Nothing
        Right tokens -> 
            case filter (covering pos) tokens of
                (t:_) -> docFor t
                [] -> Nothing

    where
    -- covering p t = 
    --     let (start, len) = tokenSpan t
    --     in p >= start && p < (start + len)

    docFor :: Token -> Maybe String
    docFor t = case t of
        METHOD _ _ -> Just "HTTP Method"
        KW_CONFIGS _ -> Just "Configuration block"
        IDENTIFIER _ "use-tls" -> Just "Config: Toggle TLS"
        _ -> Nothing

-- | Parse a token list into a 'Script' (with its semantic index), returning
-- the raw @(message, tokens)@ on failure. Shared core of 'analyze',
-- 'readScript', and 'parseWithDiagnostics'.
parseTokens :: [Token] -> IO (Either (String, [Token]) Script)
parseTokens tokens = do
    res <- runExceptT (parse tokens)
    case res of
        Left e -> pure (Left e)
        Right itemsAction -> do
            res2 <- runExceptT itemsAction
            case res2 of
                Left e -> pure (Left e)
                Right s -> pure (Right (s { index = buildIndex tokens }))

-- | Lex and parse, returning structured diagnostics (first failure) or the
-- parsed 'Script'. This is the single entry point for diagnostics consumers.
parseWithDiagnostics :: String -> IO (Either [Diagnostic] Script)
parseWithDiagnostics input =
    case scanSafe input of
        Left lexErr -> pure (Left [lexicalDiagnostic lexErr])
        Right tokens -> do
            r <- parseTokens tokens
            pure $ case r of
                Left (err, errTokens) -> Left [parseDiagnostic err errTokens]
                Right s -> Right s

-- | Lex and parse, returning diagnostics only (empty when the script is valid).
diagnostics :: String -> IO [Diagnostic]
diagnostics input = do
    r <- parseWithDiagnostics input
    pure (either id (const []) r)

lexicalDiagnostic :: LexError -> Diagnostic
lexicalDiagnostic (LexError p msg) =
    let (l, c) = posToLineCol p
    in Diagnostic (SrcSpan (l, c) (l, c + 1)) LexicalError msg (lexicalHint msg)

parseDiagnostic :: String -> [Token] -> Diagnostic
parseDiagnostic err tokens =
    let (l, c) = case tokens of
            (t : _) -> posToLineCol (getPos t)
            [] -> (1, 1)
    in Diagnostic (SrcSpan (l, c) (l, c + 1)) ParseError (firstLine err) parseHint

posToLineCol :: AlexPosn -> (Int, Int)
posToLineCol (AlexPn _ l c) = (l, c)

lexicalHint :: String -> String
lexicalHint msg
    | "unbalanced braces" `isInfixOf` msg =
        "Ensure every '{' has a matching '}'."
    | "escaped EOF" `isInfixOf` msg =
        "Close the unterminated string with a double quote."
    | otherwise =
        "Unexpected character; check the token at this position."

firstLine :: String -> String
firstLine = takeWhile (/= '\n')

parseHint :: String
parseHint = "Check the syntax against the hh200 grammar."

documentSymbols :: String -> [(String, (Int, Int))]
documentSymbols input = 
    case scanSafe input of
        Left _ -> []
        Right tokens -> findSymbols tokens

    where
    findSymbols :: [Token] -> [(String, (Int, Int))]
    findSymbols [] = []
    findSymbols (t:ts) = 
        case t of
            METHOD _ m -> case ts of
                (URL _ u : rest) -> 
                    let (AlexPn _ line col) = getPos t
                    in (m ++ " " ++ u, (line, col)) : findSymbols rest
                _ -> findSymbols ts
            KW_CAPTURES _ -> 
                let (caps, rest) = extractCaps ts
                in caps ++ findSymbols rest
            _ -> findSymbols ts

    extractCaps :: [Token] -> ([(String, (Int, Int))], [Token])
    extractCaps [] = ([], [])
    extractCaps (t:ts)
        | isBlockStart t = ([], t:ts)
        | otherwise = 
            case t of
                IDENTIFIER p s -> 
                    let (AlexPn _ l c) = p
                        (more, rest) = extractCaps ts
                    in ((s, (l, c)) : more, rest)
                _ -> extractCaps ts

    -- isBlockStart (KW_CONFIGS _) = True
    -- isBlockStart (KW_CAPTURES _) = True
    -- isBlockStart (KW_ASSERTS _) = True
    -- isBlockStart (KW_HTTP _) = True
    -- isBlockStart (METHOD _ _) = True
    -- isBlockStart _ = False

isBlockStart :: Token -> Bool
isBlockStart (KW_CONFIGS _) = True
isBlockStart (KW_CAPTURES _) = True
isBlockStart (KW_ASSERTS _) = True
isBlockStart (KW_HTTP _) = True
isBlockStart (METHOD _ _) = True
isBlockStart _ = False

-- ---------------------------------------------------------------------------
-- Semantic index (capture spans), built once at parse time.
-- ---------------------------------------------------------------------------

-- | Source spans for capture definitions and @{{name}}@ usages, computed once
-- from the token stream and attached to the parsed 'Script'.
buildIndex :: [Token] -> DocIndex
buildIndex tokens = DocIndex
    { ixCaptures = collectCaptures tokens
    , ixUsages   = collectUsages tokens
    }

-- | All capture definitions (@[Captures]@ blocks), in source order.
collectCaptures :: [Token] -> [(Text, SrcSpan)]
collectCaptures = go
  where
    go [] = []
    go (KW_CAPTURES _ : rest) = collectIds rest
    go (_ : rest) = go rest

    collectIds [] = []
    collectIds (IDENTIFIER p s : ts) =
        let AlexPn _ l c = p
        in (Text.pack s, SrcSpan (l, c) (l, c + length s)) : collectIds ts
    collectIds (t : ts)
        | isBlockStart t = go (t : ts)
        | otherwise = collectIds ts

-- | All @{{name}}@ usages in URLs and quoted strings, in source order.
collectUsages :: [Token] -> [(Text, SrcSpan)]
collectUsages = concatMap go
  where
    go (URL (AlexPn _ l c) s)    = interpolations (l, c) s
    go (QUOTED (AlexPn _ l c) s) = interpolations (l, c) s
    go _ = []

-- | Every @{{name}}@ interpolation in a string that starts at the given
-- 1-based (line, col), with its span.
interpolations :: SrcPos -> String -> [(Text, SrcSpan)]
interpolations (line, col) = go 0
  where
    go _ [] = []
    go i ('{':'{':rest) =
        let (name, afterName) = span (/= '}') rest
        in case afterName of
             '}':'}':after ->
                 let s = SrcSpan (line, col + i + 2) (line, col + i + 2 + length name)
                 in (Text.pack name, s) : go (i + 4 + length name) after
             _ -> go (i + 1) ('{':rest)
    go i (_ : xs) = go (i + 1) xs

-- ---------------------------------------------------------------------------
-- LSP semantic queries over the parsed index.
-- ---------------------------------------------------------------------------

-- | The definition span of the capture at the given 1-based position, if any.
definitionAt :: Script -> SrcPos -> Maybe SrcSpan
definitionAt script pos = do
    name <- captureAt pos (index script)
    lookup name (ixCaptures (index script))

-- | All definition and usage spans of the capture at the given position.
referencesAt :: Script -> SrcPos -> [SrcSpan]
referencesAt script pos =
    case captureAt pos (index script) of
        Nothing -> []
        Just name ->
            [ s | (n, s) <- ixCaptures (index script), n == name ]
            ++ [ s | (n, s) <- ixUsages (index script), n == name ]

captureAt :: SrcPos -> DocIndex -> Maybe Text
captureAt pos (DocIndex caps usages) =
    case [ n | (n, s) <- caps ++ usages, pos `inSpan` s ] of
        (n : _) -> Just n
        [] -> Nothing

inSpan :: SrcPos -> SrcSpan -> Bool
inSpan p (SrcSpan s e) = p >= s && p < e

-- | Does a token span cover a character offset? Used by @hover@.
covering :: Int -> Token -> Bool
covering p t =
    let (start, len) = tokenSpan t
    in p >= start && p < (start + len)

-- ??: after lsp format on save, implement Text based alternative implementation and bench
formatRange :: String -> ((Int, Int), (Int, Int)) -> [((Int, Int), (Int, Int), String)]
formatRange    input     ((startL, startC), (endL, endC)) =
    let allLines = lines input
        -- Assume single line for this test case
        targetLine = if length allLines >= startL then allLines !! (startL - 1) else ""
        -- Take substring. Column 1-based.
        -- "{\"a\":1, \"b\":2}" -> startC=1, endC=16.
        -- take (16 - 1 + 1) . drop (1 - 1)
        subStr = take (endC - startC + 1) $ drop (startC - 1) targetLine

        formatted = if not (null subStr) && head subStr == '{'  -- ??: partial fn head
                    then simpleJsonFormat subStr
                    else subStr

    -- [ ((Int, Int),       (Int, Int),   String) ]
    in [ ((startL, startC), (endL, endC), formatted) ]

simpleJsonFormat :: String -> String
simpleJsonFormat s = 
    let s1 = replaceStr "{" "{\n  " s
        s2 = replaceStr ", " ",\n  " s1
        s3 = replaceStr "}" "\n}" s2
    in s3

replaceStr :: String -> String -> String -> String
replaceStr _old _new [] = []
replaceStr old new str@(c:cs) =
    if old `isPrefixOf` str
    then new ++ replaceStr old new (drop (length old) str)
    else c : replaceStr old new cs
