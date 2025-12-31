{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Re-export lexer and parser generated code.
module Hh200.Scanner
    ( module Hh200.Scanner
    , module L
    , module P
    ) where

import Debug.Trace

import           System.Directory (doesFileExist)
import           System.Process (readProcess)
import qualified System.Info as Info

import           Control.Monad.Trans.Maybe
import           Control.Monad.IO.Class
import           Control.Exception (try, IOException)

import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Char (toLower, isDigit)
import           Data.List (isPrefixOf)

import Hh200.Types (Script(..), HostInfo(..), Snippet(..), hiHh200Conf, defaultHostInfo)
import L
import P

scriptFrom :: Snippet -> Maybe Script
scriptFrom (Snippet s) =
    let tokensOrPanic = alexScanTokens (L8.unpack s) in
    let parsed :: E Script = parse tokensOrPanic in

    case parsed of
        ParseFailed d _ -> trace (show d) Nothing
        ParseOk sole -> Just sole

readScript :: FilePath -> IO (Maybe Script)
readScript path = do
    loaded <- readFile path
    let tokensOrPanic = alexScanTokens loaded
    let parsed :: E Script = parse tokensOrPanic

    case parsed of
        ParseFailed m _ -> do
            putStrLn $ show m
            pure Nothing
        ParseOk s -> do
            pure $ Just s

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
    analyze :: a -> MaybeT IO Script
    analyzeWithHostInfo :: a -> MaybeT IO (Script, HostInfo)


instance Analyze FilePath where
    -- -> Nothing | StaticScript | SoleScript? | Script
    analyze :: FilePath -> MaybeT IO Script
    analyze path = do
        exists <- liftIO $ doesFileExist path
        MaybeT $ case exists of
            False -> do
                -- Proceeding to testOutsideWorld is unnecessary.
                pure Nothing
            _ -> do
                readScript path

    analyzeWithHostInfo :: FilePath -> MaybeT IO (Script, HostInfo)
    analyzeWithHostInfo path = do
        script <- analyze path
        hi <- liftIO gatherHostInfo
        pure (script, hi)


instance Analyze Snippet where
    -- -> Nothing | SoleScript
    analyze :: Snippet -> MaybeT IO Script
    analyze s@(Snippet _) = do
        (script, _) <- analyzeWithHostInfo s
        pure script

    analyzeWithHostInfo :: Snippet -> MaybeT IO (Script, HostInfo)
    analyzeWithHostInfo s@(Snippet _) = do
        let opt :: Maybe Script = Hh200.Scanner.scriptFrom s

        MaybeT $ case opt of
            Nothing -> do
                trace "analyzed Nothing" (return Nothing)
            Just baseScript -> do
                hi <- liftIO gatherHostInfo
                let scriptWithConfig = case hiHh200Conf hi of
                                            Just sc -> baseScript { config = sc }
                                            Nothing -> baseScript
                return (Just (scriptWithConfig, hi))

scanSafe :: String -> Either String [Token]
scanSafe str = runAlex str gather
  where
    gather = do
        t <- alexMonadScan
        case t of 
            EOF _ -> return []
            _ -> (t:) <$> gather

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
            _ -> [ "GET", "POST", "PUT", "DELETE", "PATCH", "OPTIONS", "HEAD"
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
    KW_CAPTURES p -> p
    KW_ASSERTS p -> p
    URL p _ -> p
    QUOTED p _ -> p
    BRACED p _ -> p
    RHS p _ -> p
    JSONPATH p _ -> p
    LINE p _ -> p
    EOF p -> p

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
        RHS _ s -> length s -- includes colon
        JSONPATH _ s -> length s
        LINE _ s -> length s -- includes >
        EOF _ -> 0

hover :: String -> Int -> Maybe String
hover input pos = 
    case scanSafe input of
        Left _ -> Nothing
        Right tokens -> 
            case filter (covering pos) tokens of
                (t:_) -> docFor t
                [] -> Nothing

    where
    covering p t = 
        let (start, len) = tokenSpan t
        in p >= start && p < (start + len)

    docFor :: Token -> Maybe String
    docFor t = case t of
        METHOD _ _ -> Just "HTTP Method"
        KW_CONFIGS _ -> Just "Configuration block"
        IDENTIFIER _ "use-tls" -> Just "Config: Toggle TLS"
        _ -> Nothing

diagnostics :: String -> [((Int, Int), String)]
diagnostics input = 
    case scanSafe input of
        Left err -> 
            let pos = extractPos err
            in [(pos, "Lexical error: " ++ err)]
        Right tokens ->
            case parse tokens of
                ParseOk _ -> []
                ParseFailed err errTokens ->
                    case errTokens of
                        (t:_) ->
                            let (AlexPn _ line col) = getPos t
                            in [((line, col), err)]
                        [] ->
                            -- Error at EOF or empty tokens list
                            [((1, 1), err)]

    where
    extractPos :: String -> (Int, Int)
    extractPos msg =
        let p = "lexical error at line "
        in if p `isPrefixOf` msg
            then
                let rest = drop (length p) msg
                    (l, rest2) = span isDigit rest
                    rest3 = dropWhile (not . isDigit) rest2
                    (c, _) = span isDigit rest3
                in case (reads l, reads c) of
                    ([(ln, _)], [(cn, _)]) -> (ln, cn)
                    _ -> (1, 1)
            else (1, 1)

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

    isBlockStart (KW_CONFIGS _) = True
    isBlockStart (KW_CAPTURES _) = True
    isBlockStart (KW_ASSERTS _) = True
    isBlockStart (KW_HTTP _) = True
    isBlockStart (METHOD _ _) = True
    isBlockStart _ = False

isBlockStart :: Token -> Bool
isBlockStart (KW_CONFIGS _) = True
isBlockStart (KW_CAPTURES _) = True
isBlockStart (KW_ASSERTS _) = True
isBlockStart (KW_HTTP _) = True
isBlockStart (METHOD _ _) = True
isBlockStart _ = False

definition :: String -> Int -> Maybe ((Int, Int), (Int, Int))
definition input pos = 
    case scanSafe input of
        Right tokens -> 
            case findTarget tokens pos of
                Just name -> resolveDefinition name tokens
                Nothing -> Nothing
        Left _ -> Nothing

references :: String -> Int -> [((Int, Int), (Int, Int))]
references input pos = 
    case scanSafe input of
        Right tokens -> 
            case findTarget tokens pos of
                Just name -> findReferences name tokens
                Nothing -> []
        Left _ -> []

rename :: String -> Int -> String -> [((Int, Int), (Int, Int), String)]
rename input pos newName =
    case scanSafe input of
        Right tokens -> 
            case findTarget tokens pos of
                Just name -> 
                    let refs = findReferences name tokens
                    in [ (start, end, newName) | (start, end) <- refs ]
                Nothing -> []
        Left _ -> []

-- Shared Helpers

findTarget :: [Token] -> Int -> Maybe String
findTarget tokens p = 
    case filter (covering p) tokens of
        (t:_) -> extractNameFromToken t p
        [] -> Nothing

covering :: Int -> Token -> Bool
covering p t = 
    let (start, len) = tokenSpan t
    in p >= start && p < (start + len)

extractNameFromToken :: Token -> Int -> Maybe String
extractNameFromToken t p = case t of
    IDENTIFIER _ s -> Just s
    URL (AlexPn offset _ _) s -> checkInterpolation s (p - offset)
    QUOTED (AlexPn offset _ _) s -> checkInterpolation s (p - offset)
    _ -> Nothing

checkInterpolation :: String -> Int -> Maybe String
checkInterpolation s relPos = findVar s 0
    where
    findVar [] _ = Nothing
    findVar ('{':'{':rest) i =
        let (name, remainder) = span (`notElem` ("}" :: String)) rest
        in if length remainder >= 2 && take 2 remainder == "}}"
           then 
               let start = i
                   end = i + 4 + length name
               in if relPos >= start && relPos < end
                  then Just name
                  else findVar remainder (i + 2 + length name)
           else findVar rest (i+1)
    findVar (_:xs) i = findVar xs (i+1)

resolveDefinition :: String -> [Token] -> Maybe ((Int, Int), (Int, Int))
resolveDefinition name tokens = 
    let captures = extractCapturesWithPos tokens
    in case lookup name captures of
        Just (l, c) -> Just ((l, c), (l, c + length name))
        Nothing -> Nothing

findReferences :: String -> [Token] -> [((Int, Int), (Int, Int))]
findReferences name tokens = 
    let defs = findDefinitionOccurrences name tokens
        usages = findUsageOccurrences name tokens
    in defs ++ usages

findDefinitionOccurrences :: String -> [Token] -> [((Int, Int), (Int, Int))]
findDefinitionOccurrences name tokens = 
    let captures = extractCapturesWithPos tokens
    in [ ((l, c), (l, c + length name)) | (n, (l, c)) <- captures, n == name ]

findUsageOccurrences :: String -> [Token] -> [((Int, Int), (Int, Int))]
findUsageOccurrences name [] = []
findUsageOccurrences name (t:ts) = 
    case t of
        URL (AlexPn _ line col) s -> 
            let offsets = findOffsets name s
            in [ ((line, col + off), (line, col + off + length name)) | off <- offsets ] ++ findUsageOccurrences name ts
        QUOTED (AlexPn _ line col) s ->
            let offsets = findOffsets name s
            in [ ((line, col + off), (line, col + off + length name)) | off <- offsets ] ++ findUsageOccurrences name ts
        _ -> findUsageOccurrences name ts

findOffsets :: String -> String -> [Int]
findOffsets name s = scan s 0
    where
    scan [] _ = []
    scan ('{':'{':rest) i =
        let (n, remainder) = span (`notElem` ("}" :: String)) rest
        in if length remainder >= 2 && take 2 remainder == "}}"
           then 
               if n == name
               then (i + 2) : scan remainder (i + 4 + length n)
               else scan remainder (i + 4 + length n)
           else scan rest (i+1)
    scan (_:xs) i = scan xs (i+1)

extractCapturesWithPos :: [Token] -> [(String, (Int, Int))]
extractCapturesWithPos [] = []
extractCapturesWithPos (KW_CAPTURES _ : rest) = collectIdsForCaps rest
extractCapturesWithPos (_:rest) = extractCapturesWithPos rest

collectIdsForCaps :: [Token] -> [(String, (Int, Int))]
collectIdsForCaps (IDENTIFIER p s : ts) = 
    let (AlexPn _ l c) = p
    in (s, (l, c)) : collectIdsForCaps ts
collectIdsForCaps (t:ts)
    | isBlockStart t = extractCapturesWithPos (t:ts)
    | otherwise = collectIdsForCaps ts
collectIdsForCaps [] = []

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

        formatted = if not (null subStr) && head subStr == '{'
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
replaceStr old new [] = []
replaceStr old new str@(c:cs) =
    if old `isPrefixOf` str
    then new ++ replaceStr old new (drop (length old) str)
    else c : replaceStr old new cs
