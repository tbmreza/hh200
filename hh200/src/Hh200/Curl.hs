{-# LANGUAGE OverloadedStrings #-}

-- | Run a curl snippet without a shell.
--
-- A snippet is a fixed subset of bash: @NAME=value@ assignments (including
-- @NAME=$(base64 -w 0 PATH)@), and exactly one @curl@ command, possibly split
-- with @\\@-newline and fed a heredoc. It is lowered onto the same 'CallItem'
-- a @.hhs@ script produces, so execution, captures and the dashboard work
-- unchanged. Anything outside the subset is a positioned 'Diagnostic'; nothing
-- is ever handed to @/bin/sh@.
--
-- How the subset maps onto hh200:
--
-- * An assignment becomes a @[Vars]@ entry; @$(base64 -w 0 PATH)@ becomes
--   @base64-file,PATH@, so hh200 reads the file itself.
-- * @$NAME@ becomes @{{NAME}}@, bound by the snippet or from outside
--   (@--var@, @HH200_VAR_NAME@).
-- * Flags follow curl: @-X@ wins, data implies POST, @-d@ without a
--   @Content-Type@ adds curl's form default, @--json@ adds JSON headers.
module Hh200.Curl
    ( -- * Shared with the curl conformance harness (@CurlTest.Translate@)
      shellWords
    , curlMethod
    , curlDataArg
      -- * Snippets
    , Piece (..)
    , AssignValue (..)
    , Assignment (..)
    , CurlRequest (..)
    , DataSource (..)
    , CurlSnippet (..)
    , parseCurlSnippet
    , lowerCurl
    , curlToScript
    , emitHhs
    ) where

import           Control.Monad (foldM, forM, unless, when)
import           Data.Char (isAlpha, isAlphaNum, isSpace)
import qualified Data.HashMap.Strict as HM
import           Data.List (isInfixOf, isPrefixOf, sortOn)
import           Data.Maybe (fromMaybe, isJust, listToMaybe)
import qualified Data.Text as Text

import qualified BEL
import           Hh200.Types

-- ---------------------------------------------------------------------------
-- Shared with the conformance harness
-- ---------------------------------------------------------------------------

-- | Split a command line into words the way a shell would, leniently: single
-- quotes are literal, double quotes keep spaces (with @\\@ escaping @"@, @\\@,
-- @$@ and backtick), and an unquoted @\\@ escapes the next character. No
-- expansion. An unterminated quote runs to the end of the input.
shellWords :: String -> [String]
shellWords = go . dropWhile isSpace
  where
    go [] = []
    go s = let (w, rest) = wordOf s in w : go (dropWhile isSpace rest)

    wordOf [] = ([], [])
    wordOf s@(c : cs)
        | isSpace c = ([], s)
        | c == '\'' = let (inside, r) = break (== '\'') cs in prepend inside (wordOf (drop 1 r))
        | c == '"' = let (inside, r) = dquoted cs in prepend inside (wordOf r)
        | c == '\\', (x : r) <- cs = prepend [x] (wordOf r)
        | otherwise = prepend [c] (wordOf cs)

    dquoted [] = ([], [])
    dquoted ('"' : r) = ([], r)
    dquoted ('\\' : x : r) | x `elem` ("\"\\$`" :: String) = prepend [x] (dquoted r)
    dquoted (x : r) = prepend [x] (dquoted r)

    prepend p (w, r) = (p ++ w, r)

dataFlags :: [String]
dataFlags = ["-d", "--data", "--data-ascii", "--data-binary", "--data-raw", "--json"]

-- | The method curl would use for these arguments: @-X@/@--request@ wins,
-- then @-T@ implies PUT, then a data flag implies POST, else GET.
curlMethod :: [String] -> String
curlMethod args =
    fromMaybe implied explicit
  where
    explicit = listToMaybe ([ m | (f, m) <- pairs, f `elem` ["-X", "--request"] ]
                            ++ [ drop 2 a | a <- args, "-X" `isPrefixOf` a, length a > 2 ]
                            ++ [ drop 10 a | a <- args, "--request=" `isPrefixOf` a ])
    implied
        | "-T" `elem` args = "PUT"
        | any isDataFlag args = "POST"
        | otherwise = "GET"
    isDataFlag a = a `elem` dataFlags || any (\f -> (f ++ "=") `isPrefixOf` a) (drop 1 dataFlags)
    pairs = zip args (drop 1 args)

-- | The value of the first data flag (@-d@, @--data@, @--data-binary@, ...).
curlDataArg :: [String] -> Maybe String
curlDataArg args = listToMaybe [ v | (f, v) <- zip args (drop 1 args), f `elem` dataFlags ]

-- ---------------------------------------------------------------------------
-- Snippet syntax
-- ---------------------------------------------------------------------------

-- | A piece of a word after quoting is resolved.
data Piece
    = Lit String  -- ^ literal text
    | Ref String  -- ^ @$NAME@ or @${NAME}@
    deriving (Eq, Show)

data AssignValue
    = AssignPieces [Piece]          -- ^ @NAME=text with $REFS@
    | AssignBase64File FilePath     -- ^ @NAME=$(base64 -w 0 PATH)@
    deriving (Eq, Show)

data Assignment = Assignment
    { asName  :: String
    , asValue :: AssignValue
    } deriving (Eq, Show)

data DataSource
    = DataPieces [Piece]   -- ^ a literal @-d@ value
    | DataStdin [Piece]    -- ^ @-d \@-@ fed by a heredoc
    deriving (Eq, Show)

-- | The curl command, with flags interpreted.
data CurlRequest = CurlRequest
    { crMethod  :: String
    , crUrl     :: [Piece]
    , crHeaders :: [(String, [Piece])]
    , crData    :: [DataSource]
    , crJson    :: Bool  -- ^ @--json@ was used
    } deriving (Eq, Show)

data CurlSnippet = CurlSnippet
    { csAssignments :: [Assignment]
    , csRequest     :: CurlRequest
    } deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Lexer
-- ---------------------------------------------------------------------------

type PChar = (Char, SrcPos)

-- | A word piece before assignments are recognised; command substitutions are
-- kept raw and only accepted in the one assignment form.
data WPiece = WLit String | WRef String | WSubst SrcPos String
    deriving (Eq, Show)

data Tok
    = TWord SrcPos [WPiece]
    | THeredocOp SrcPos
    | THeredoc [Piece]
    | TNewline
    deriving (Eq, Show)

type LexResult a = Either (SrcPos, String) a

data Pending = Pending
    { pdDelim     :: String
    , pdQuoted    :: Bool  -- ^ quoted delimiter: no @$@ expansion
    , pdStripTabs :: Bool  -- ^ @<<-@
    , pdPos       :: SrcPos
    }

annotate :: String -> [PChar]
annotate = go 1 1
  where
    go _ _ [] = []
    go l c (ch : rest)
        | ch == '\n' = (ch, (l, c)) : go (l + 1) 1 rest
        | otherwise  = (ch, (l, c)) : go l (c + 1) rest

opChars :: String
opChars = "|;&<>()`"

opMessage :: String -> String
opMessage op = case op of
    "|"  -> "pipes ('|') aren't supported: hh200 reads assignments and one curl command, it doesn't run a shell"
    "||" -> "'||' isn't supported: hh200 reads assignments and one curl command, it doesn't run a shell"
    "&&" -> "'&&' isn't supported: put each assignment on its own line"
    ";"  -> "';' isn't supported: put each assignment on its own line"
    "&"  -> "background jobs ('&') aren't supported"
    "`"  -> "backtick command substitution isn't supported; only NAME=$(base64 -w 0 PATH) is"
    _    -> "'" ++ op ++ "' isn't supported: the only redirection hh200 reads is a heredoc for --data-binary @-"

lexShell :: String -> LexResult [Tok]
lexShell src = go (annotate src) []
  where
    go :: [PChar] -> [Pending] -> LexResult [Tok]
    go [] [] = Right []
    go [] (p : _) = Left (pdPos p, "heredoc '" ++ pdDelim p ++ "' has no closing delimiter line")
    go s@((c, p) : rest) pend
        | c == '\n' = do
            (docs, rest') <- readHeredocs pend rest
            more <- go rest' []
            Right (map THeredoc docs ++ TNewline : more)
        | c `elem` (" \t\r" :: String) = go rest pend
        | c == '\\', (('\n', _) : rest2) <- rest = go rest2 pend
        | c == '#' = go (dropWhile ((/= '\n') . fst) rest) pend
        | c == '<', (('<', _) : rest2) <- rest = heredocOp p rest2 pend
        | c `elem` opChars = Left (p, opMessage (opAt s))
        | otherwise = do
            (w, rest') <- word s
            more <- go rest' pend
            Right (TWord p w : more)

    opAt ((a, _) : (b, _) : _) | [a, b] `elem` ["||", "&&"] = [a, b]
    opAt ((a, _) : _) = [a]
    opAt [] = ""

    heredocOp p s0 pend = do
        let (strip, s1) = case s0 of
                (('-', _) : r) -> (True, r)
                r -> (False, r)
            s2 = dropWhile ((`elem` (" \t" :: String)) . fst) s1
            quoted = case s2 of
                ((q, _) : _) -> q `elem` ("'\"" :: String)
                [] -> False
        case s2 of
            ((d, _) : _) | d `notElem` ('\n' : opChars) -> Right ()
            _ -> Left (p, "'<<' needs a delimiter word, as in <<EOF")
        (w, rest) <- word s2
        delim <- case w of
            ps | all isLit ps -> Right (concat [ t | WLit t <- ps ])
            _ -> Left (p, "a heredoc delimiter can't contain $ or $(...)")
        more <- go rest (pend ++ [Pending delim quoted strip p])
        Right (THeredocOp p : more)

    isLit (WLit _) = True
    isLit _ = False

    -- Read the bodies of the heredocs opened on the line just ended.
    readHeredocs :: [Pending] -> [PChar] -> LexResult ([[Piece]], [PChar])
    readHeredocs [] s = Right ([], s)
    readHeredocs (p : ps) s = do
        (body, rest) <- readOne p s []
        (bodies, rest') <- readHeredocs ps rest
        Right (body : bodies, rest')

    readOne :: Pending -> [PChar] -> [[PChar]] -> LexResult ([Piece], [PChar])
    readOne p [] _ = Left (pdPos p, "heredoc '" ++ pdDelim p ++ "' has no closing delimiter line")
    readOne p s acc =
        let (line, rest) = break ((== '\n') . fst) s
            line' = if pdStripTabs p then dropWhile ((== '\t') . fst) line else line
            rest' = drop 1 rest
        in if map fst line' == pdDelim p
            then do
                let bodyChars = concatMap (++ [('\n', (0, 0))]) (reverse acc)
                body <- if pdQuoted p then Right [Lit (map fst bodyChars)] else expandHeredoc bodyChars
                Right (body, rest')
            else if null rest
                then Left (pdPos p, "heredoc '" ++ pdDelim p ++ "' has no closing delimiter line")
                else readOne p rest' (line' : acc)

-- | @$NAME@ / @${NAME}@ expansion in an unquoted-delimiter heredoc body.
expandHeredoc :: [PChar] -> LexResult [Piece]
expandHeredoc = fmap mergePieces . go
  where
    go [] = Right []
    go (('\\', _) : (x, _) : r) | x `elem` ("$`\\" :: String) = (Lit [x] :) <$> go r
    go (('\\', _) : ('\n', _) : r) = go r
    go (('$', p) : r) = do
        (piece, r') <- dollar p r
        case piece of
            WRef n -> (Ref n :) <$> go r'
            WLit t -> (Lit t :) <$> go r'
            WSubst sp _ -> Left (sp, "command substitution inside a heredoc isn't supported; assign it to a variable first, as in IMG=$(base64 -w 0 photo.png)")
    go (('`', p) : _) = Left (p, opMessage "`")
    go ((c, _) : r) = (Lit [c] :) <$> go r

-- | One word: quotes, escapes, @$NAME@ and @$(...)@ resolved into pieces.
word :: [PChar] -> LexResult ([WPiece], [PChar])
word = loop []
  where
    loop acc [] = Right (finish acc, [])
    loop acc s@((c, p) : rest)
        | c `elem` (" \t\r\n" :: String) = Right (finish acc, s)
        | c `elem` opChars = Right (finish acc, s)
        | c == '\\' = case rest of
            (('\n', _) : r) -> loop acc r
            ((x, _) : r) -> loop (WLit [x] : acc) r
            [] -> loop (WLit "\\" : acc) []
        | c == '\'' = case break ((== '\'') . fst) rest of
            (inside, _ : r) -> loop (WLit (map fst inside) : acc) r
            (_, []) -> Left (p, "unterminated single quote")
        | c == '"' = do
            (ps, r) <- dquote p rest []
            loop (reverse ps ++ acc) r
        | c == '$' = do
            (piece, r) <- dollar p rest
            loop (piece : acc) r
        | otherwise = loop (WLit [c] : acc) rest

    finish = mergeW . reverse

    dquote open [] _ = Left (open, "unterminated double quote")
    dquote _ (('"', _) : r) acc = Right (reverse acc, r)
    dquote open (('\\', _) : (x, _) : r) acc
        | x `elem` ("$`\"\\" :: String) = dquote open r (WLit [x] : acc)
        | x == '\n' = dquote open r acc
    dquote open (('$', p) : r) acc = do
        (piece, r') <- dollar p r
        dquote open r' (piece : acc)
    dquote _ (('`', p) : _) _ = Left (p, opMessage "`")
    dquote open ((x, _) : r) acc = dquote open r (WLit [x] : acc)

-- | After a @$@: a reference, a command substitution, or a literal @$@.
dollar :: SrcPos -> [PChar] -> LexResult (WPiece, [PChar])
dollar p s = case s of
    (('{', _) : r) ->
        let (name, after) = break ((== '}') . fst) r
            n = map fst name
        in case after of
            (_ : r') | validName n -> Right (WRef n, r')
            _ -> Left (p, "only ${NAME} is supported, not ${" ++ n ++ "...}")
    (('(', _) : r) -> do
        (content, r') <- balanced p r
        Right (WSubst p content, r')
    ((c, _) : _) | isAlpha c || c == '_' ->
        let (name, r') = span (\(x, _) -> isAlphaNum x || x == '_') s
        in Right (WRef (map fst name), r')
    _ -> Right (WLit "$", s)

-- | The text up to the @)@ closing a @$(@, respecting nested parens and quotes.
balanced :: SrcPos -> [PChar] -> LexResult (String, [PChar])
balanced open = go (1 :: Int) []
  where
    go _ _ [] = Left (open, "unterminated $(")
    go depth acc ((c, _) : r)
        | c == ')' && depth == 1 = Right (reverse acc, r)
        | c == ')' = go (depth - 1) (c : acc) r
        | c == '(' = go (depth + 1) (c : acc) r
        | c == '\'' || c == '"' =
            let (inside, after) = break ((== c) . fst) r
            in case after of
                (_ : r') -> go depth (c : reverse (map fst inside) ++ c : acc) r'
                [] -> Left (open, "unterminated quote inside $(")
        | otherwise = go depth (c : acc) r

mergeW :: [WPiece] -> [WPiece]
mergeW (WLit a : WLit b : r) = mergeW (WLit (a ++ b) : r)
mergeW (x : r) = x : mergeW r
mergeW [] = []

mergePieces :: [Piece] -> [Piece]
mergePieces (Lit a : Lit b : r) = mergePieces (Lit (a ++ b) : r)
mergePieces (Lit "" : r) = mergePieces r
mergePieces (x : r) = x : mergePieces r
mergePieces [] = []

validName :: String -> Bool
validName n@(c : _) = (isAlpha c || c == '_') && all (\x -> isAlphaNum x || x == '_') n
validName [] = False

-- ---------------------------------------------------------------------------
-- Parser
-- ---------------------------------------------------------------------------

data Command
    = CAssign SrcPos Assignment
    | CCurl SrcPos [(SrcPos, [WPiece])] [[Piece]]  -- ^ args, heredoc bodies

type Fail = (SrcPos, String)

-- | Parse a snippet into its assignments and interpreted curl command.
parseCurlSnippet :: String -> Either Diagnostic CurlSnippet
parseCurlSnippet src = either (Left . toDiag) Right $ do
    toks <- lexShell (crlfToLf src)
    cmds <- mapM command (filter (not . null) (splitLines toks))
    let assigns = [ a | CAssign _ a <- cmds ]
        curls = [ (p, args, docs) | CCurl p args docs <- cmds ]
    case curls of
        [] -> Left ((1, 1), "no curl command found")
        (_ : (p2, _, _) : _) -> Left (p2, "only one curl command is supported per snippet")
        [(p, args, docs)] -> do
            checkOrder cmds
            req <- interpretCurl p args docs
            Right (CurlSnippet assigns req)
  where
    crlfToLf ('\r' : '\n' : r) = '\n' : crlfToLf r
    crlfToLf (x : r) = x : crlfToLf r
    crlfToLf [] = []

    toDiag (pos@(l, c), msg) = Diagnostic (SrcSpan pos (l, c + 1)) ParseError msg
        "hh200 reads NAME=... assignments and one curl command (see `hh200 --help`)."

    splitLines ts = case break (== TNewline) ts of
        (line, []) -> [line]
        (line, _ : rest) -> line : splitLines rest

    -- Assignments must come before the curl command that uses them.
    checkOrder cmds = case dropWhile isAssign cmds of
        (_ : rest) | (CAssign p _ : _) <- filter isAssign rest ->
            Left (p, "assignments after the curl command aren't supported")
        _ -> Right ()
    isAssign (CAssign _ _) = True
    isAssign _ = False

command :: [Tok] -> Either Fail Command
command toks = case words' of
    ((p, [WLit "export"]) : rest) -> assignOnly p rest
    ((p, w) : rest) | isJust (assignmentName w) -> assignOnly p ((p, w) : rest)
    ((p, [WLit "curl"]) : args) -> Right (CCurl p args heredocs)
    ((p, w) : _) -> Left (p, "unsupported command '" ++ render w ++ "': hh200 reads NAME=... assignments and one curl command")
    [] -> case [ p | THeredocOp p <- toks ] of
        (p : _) -> Left (p, "a heredoc must follow the curl command")
        [] -> Left ((1, 1), "empty command")
  where
    words' = [ (p, w) | TWord p w <- toks ]
    heredocs = [ b | THeredoc b <- toks ]
    render = concatMap (\x -> case x of
        WLit t -> t
        WRef n -> "$" ++ n
        WSubst _ s -> "$(" ++ s ++ ")")

    assignOnly p [(_, w)] = do
        unless (null heredocs) $ Left (p, "a heredoc must follow the curl command")
        a <- assignment p w
        Right (CAssign p a)
    assignOnly p [] = Left (p, "'export' needs NAME=value")
    assignOnly _ (_ : (p2, _) : _) =
        Left (p2, "a command after an assignment (NAME=x cmd) isn't supported; put the assignment on its own line")

assignmentName :: [WPiece] -> Maybe (String, String)
assignmentName (WLit t : _) = case break (== '=') t of
    (name, '=' : rest) | validName name -> Just (name, rest)
    _ -> Nothing
assignmentName _ = Nothing

assignment :: SrcPos -> [WPiece] -> Either Fail Assignment
assignment p w = case (assignmentName w, w) of
    (Just (name, firstRest), _ : more) -> do
        let pieces = mergeW (filter (/= WLit "") (WLit firstRest : more))
        value <- case pieces of
            [WSubst sp content] -> AssignBase64File <$> base64Subst sp content
            ps | any isSubst ps -> Left (substPos ps, "a command substitution must be the whole value, as in IMG=$(base64 -w 0 photo.png)")
               | otherwise -> Right (AssignPieces (map toPiece ps))
        Right (Assignment name value)
    _ -> Left (p, "expected NAME=value")
  where
    isSubst (WSubst _ _) = True
    isSubst _ = False
    substPos ps = case [ sp | WSubst sp _ <- ps ] of
        (sp : _) -> sp
        [] -> p
    toPiece (WLit t) = Lit t
    toPiece (WRef n) = Ref n
    toPiece (WSubst _ s) = Lit s  -- unreachable: filtered above

-- | The one command substitution hh200 understands: base64 of a file.
base64Subst :: SrcPos -> String -> Either Fail FilePath
base64Subst p content = case shellWords content of
    ["base64", "-w", "0", path] -> ok path
    ["base64", "-w0", path] -> ok path
    ["base64", "--wrap=0", path] -> ok path
    ["base64", "-i", path] -> ok path
    ["base64", "<", path] -> ok path
    ["base64", path] | not ("-" `isPrefixOf` path) ->
        Left (p, "use $(base64 -w 0 " ++ path ++ "): plain base64 wraps lines on Linux")
    _ -> Left (p, "only $(base64 -w 0 PATH) (or -w0, -i PATH, < PATH) is supported as a command substitution")
  where
    ok path
        | '$' `elem` path = Left (p, "variables in the base64 path aren't supported")
        | otherwise = Right path

-- ---------------------------------------------------------------------------
-- curl flags
-- ---------------------------------------------------------------------------

ignoredFlags :: [String]
ignoredFlags =
    [ "-s", "-S", "-v", "-L", "-f", "-i"
    , "--silent", "--show-error", "--verbose", "--location", "--fail", "--include" ]

interpretCurl :: SrcPos -> [(SrcPos, [WPiece])] -> [[Piece]] -> Either Fail CurlRequest
interpretCurl curlPos args docs = do
    pargs <- forM args $ \(p, w) -> case [ sp | WSubst sp _ <- w ] of
        (sp : _) -> Left (sp, "command substitution in curl arguments isn't supported; assign it to a variable first")
        [] -> Right (p, mergePieces (map toPiece w))
    st <- go pargs initial
    url <- maybe (Left (curlPos, "the curl command has no URL")) Right (stUrl st)
    when (stStdinUsed st && null docs) $
        Left (curlPos, "--data-binary @- reads stdin, so it needs a heredoc (<<EOF ... EOF)")
    unless (stStdinUsed st || null docs) $
        Left (curlPos, "a heredoc is only read as the body of --data-binary @-")
    when (length docs > 1) $
        Left (curlPos, "only one heredoc is supported")
    let flagWords = map (renderFlat . snd) args
        stdinBody = case docs of
            (d : _) -> d
            [] -> []
        datas = [ case d of DataStdin _ -> DataStdin stdinBody; other -> other | d <- reverse (stData st) ]
    Right CurlRequest
        { crMethod = fromMaybe (curlMethod flagWords) (stMethod st)
        , crUrl = url
        , crHeaders = reverse (stHeaders st)
        , crData = datas
        , crJson = stJson st
        }
  where
    toPiece (WLit t) = Lit t
    toPiece (WRef n) = Ref n
    toPiece (WSubst _ s) = Lit s

    initial = St Nothing Nothing [] [] False False

    go [] st = Right st
    go ((p, w) : rest) st = case literal w of
        Just "--" -> foldM (\s (q, u) -> setUrl q u s) st rest
        Just f
            | f `elem` ["-X", "--request"] -> withValue p f rest $ \v r -> do
                m <- literalOr p "the method" v
                go r st { stMethod = Just m }
            | Just m <- longValue "--request" f -> go rest st { stMethod = Just m }
            | "-X" `isPrefixOf` f, length f > 2 -> go rest st { stMethod = Just (drop 2 f) }
            | f `elem` ["-H", "--header"] -> withValue p f rest $ \v r -> header p v st >>= go r
            | Just h <- longValue "--header" f -> header p [Lit h] st >>= go rest
            | f `elem` dataFlags -> withValue p f rest $ \v r -> dataArg p f v st >>= go r
            | Just (fl, v) <- longData f -> dataArg p fl [Lit v] st >>= go rest
            | "-d" `isPrefixOf` f, length f > 2 -> dataArg p "-d" [Lit (drop 2 f)] st >>= go rest
            | f == "--url" -> withValue p f rest $ \v r -> setUrl p v st >>= go r
            | Just u <- longValue "--url" f -> setUrl p [Lit u] st >>= go rest
            | f `elem` ignoredFlags || isShortCombo f -> go rest st
            | "-" `isPrefixOf` f -> Left (p, "unsupported curl flag " ++ f
                ++ " (hh200 reads -X, -H, -d/--data/--data-raw/--data-binary/--json, --url, and ignores -s -S -v -L -f -i)")
        _ -> setUrl p w st >>= go rest

    withValue p f rest k = case rest of
        ((_, v) : r) -> k v r
        [] -> Left (p, f ++ " needs a value")

    literal [Lit t] = Just t
    literal _ = Nothing

    literalOr p what v = maybe (Left (p, what ++ " must be literal text")) Right (literal v)

    longValue name f = if (name ++ "=") `isPrefixOf` f then Just (drop (length name + 1) f) else Nothing
    longData f = listToMaybe [ (fl, v) | fl <- drop 1 dataFlags, Just v <- [longValue fl f] ]
    isShortCombo f = case f of
        ('-' : cs@(_ : _ : _)) -> all (`elem` ("sSvLfi" :: String)) cs
        _ -> False

    setUrl p u st = case stUrl st of
        Just _ -> Left (p, "more than one URL; hh200 sends one request per snippet")
        Nothing -> Right st { stUrl = Just (defaultScheme u) }

    defaultScheme u = case u of
        (Lit t : _) | "://" `isInfixOf` t -> u
        (Lit t : r) -> Lit ("http://" ++ t) : r
        _ -> u

    header p v st = do
        (name, value) <- splitHeader p v
        when (any ((== lower name) . lower . fst) (stHeaders st)) $
            Left (p, "header " ++ name ++ " is given twice; hh200 sends one value per header")
        Right st { stHeaders = (name, value) : stHeaders st }

    lower = map (\c -> if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c)

    dataArg p f v st = do
        src <- case v of
            [Lit "@-"] | f /= "--data-raw" -> Right (DataStdin [])
            (Lit ('@' : _) : _) | f /= "--data-raw" ->
                Left (p, "reading a body from a file (" ++ f ++ " @FILE) isn't supported yet; use a heredoc with @-")
            _ -> Right (DataPieces v)
        when (isStdin src && stStdinUsed st) $ Left (p, "@- is given twice")
        Right st { stData = src : stData st
                 , stStdinUsed = stStdinUsed st || isStdin src
                 , stJson = stJson st || f == "--json" }
    isStdin (DataStdin _) = True
    isStdin _ = False

-- | Split @Name: value@. The name must be literal.
splitHeader :: SrcPos -> [Piece] -> Either Fail (String, [Piece])
splitHeader p pieces = case pieces of
    (Lit t : rest) | (name, ':' : v) <- break (== ':') t, not (null name) -> do
        let value = mergePieces (Lit (dropWhile (== ' ') v) : rest)
        when (null value || value == [Lit ""]) $
            Left (p, "an empty header value (curl's way to remove a header) isn't supported")
        unless (all (\c -> isAlphaNum c || c `elem` ("-_" :: String)) name) $
            Left (p, "unsupported header name '" ++ name ++ "'")
        Right (name, value)
    _ -> Left (p, "a header must look like 'Name: value' with a literal name")

data St = St
    { stMethod    :: Maybe String
    , stUrl       :: Maybe [Piece]
    , stHeaders   :: [(String, [Piece])]  -- reversed
    , stData      :: [DataSource]         -- reversed
    , stStdinUsed :: Bool
    , stJson      :: Bool
    }

renderFlat :: [WPiece] -> String
renderFlat = concatMap (\x -> case x of
    WLit t -> t
    WRef n -> "$" ++ n
    WSubst _ s -> "$(" ++ s ++ ")")

-- ---------------------------------------------------------------------------
-- Lowering
-- ---------------------------------------------------------------------------

-- | Lower a parsed snippet onto a 'CallItem', with warnings worth showing.
lowerCurl :: CurlSnippet -> Either String (CallItem, [String])
lowerCurl (CurlSnippet assigns req) = do
    vars <- lowerAssignments assigns
    urlText <- templ "the URL" (crUrl req)
    headers <- forM (crHeaders req) $ \(n, v) -> do
        _ <- templ ("header " ++ n) v
        Right (n, map toPart v)
    body <- lowerBody (crData req)
    let has n = any ((== lowerS n) . lowerS . fst) headers
        isForm = not (null (crData req)) && not (crJson req)
        defaults =
            [ ("Content-Type", [BEL.R "application/json"]) | crJson req, not (has "Content-Type") ]
            ++ [ ("Accept", [BEL.R "application/json"]) | crJson req, not (has "Accept") ]
            ++ [ ("Content-Type", [BEL.R "application/x-www-form-urlencoded"]) | isForm, not (has "Content-Type") ]
        allHeaders = headers ++ defaults
        url = if any isRef (crUrl req)
            then LexedUrlSegments (BEL.partitions (Text.pack urlText))
            else LexedUrlFull urlText
        squares = if null vars then rqSquaresNothing
                  else rqSquaresNothing { sqVars = Just (RequestSquareVars (RhsDict (HM.fromList vars))) }
        spec = RequestSpec
            { rqMethod = crMethod req
            , rqSquares = squares
            , rqUrl = url
            , rqHeaders = RhsDict (HM.fromList [ (Text.pack n, v) | (n, v) <- allHeaders ])
            , rqBody = body
            }
        warnings =
            [ "the Authorization header holds a literal credential; write it as $NAME and pass it with --var NAME=... or HH200_VAR_NAME"
            | (n, v) <- crHeaders req, lowerS n == "authorization", not (any isRef v) ]
    Right (CallItem { ciDeps = [], ciName = "", ciRequestSpec = spec, ciResponseSpec = Nothing }, warnings)
  where
    lowerS = map (\c -> if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c)
    isRef (Ref _) = True
    isRef _ = False

    toPart (Lit t) = BEL.R (Text.pack t)
    toPart (Ref n) = BEL.L (Text.pack n)

    -- Text with $REFS as {{REFS}}; a literal "{{" would be read as a template.
    templ what ps = do
        when (any (\x -> case x of Lit t -> "{{" `isInfixOf` t; _ -> False) ps) $
            Left (what ++ " contains '{{', which hh200 would read as a template")
        Right (concatMap (\x -> case x of Lit t -> t; Ref n -> "{{" ++ n ++ "}}") ps)

    lowerBody ds = do
        parts <- forM ds $ \d -> case d of
            DataPieces ps -> templ "the body" ps
            DataStdin ps -> templ "the heredoc body" ps
        Right (joinWith "&" parts)

    joinWith _ [] = ""
    joinWith sep (x : xs) = x ++ concatMap (sep ++) xs

    -- [Vars] entries can't see each other, so a reference to an earlier
    -- literal assignment is inlined; anything else stays a {{NAME}}.
    lowerAssignments :: [Assignment] -> Either String [(Text.Text, [BEL.Part])]
    lowerAssignments = fmap (reverse . snd) . foldM step ([], [])
      where
        step (literals, acc) (Assignment name value) = case value of
            AssignBase64File path ->
                Right (literals, (Text.pack name, [BEL.R (Text.pack ("base64-file," ++ path))]) : acc)
            AssignPieces ps -> do
                inlined <- fmap concat $ forM ps $ \x -> case x of
                    Ref n | Just lit <- lookup n literals -> Right [Lit lit]
                    Ref n | any ((== Text.pack n) . fst) acc ->
                        Left ("assignment " ++ name ++ " uses $" ++ n ++ ", which is itself built from a file or another variable; hh200 can't chain those yet")
                    other -> Right [other]
                let merged = mergePieces inlined
                    literals' = case merged of
                        [Lit t] -> (name, t) : literals
                        [] -> (name, "") : literals
                        _ -> literals
                _ <- templ ("assignment " ++ name) merged
                Right (literals', (Text.pack name, map toPart merged) : acc)

-- | Parse and lower a snippet into a one-request 'Script'.
curlToScript :: String -> Either Diagnostic (Script, [String])
curlToScript src = do
    snippet <- parseCurlSnippet src
    (ci, warnings) <- either (\e -> Left (Diagnostic (SrcSpan (1, 1) (1, 2)) ParseError e "")) Right (lowerCurl snippet)
    Right (Script { config = defaultScriptConfig, callItems = [ci], index = emptyDocIndex }, warnings)

-- | The @.hhs@ equivalent of a lowered call item, when @.hhs@ can express it:
-- the URL must start with @http@ and the body must be a JSON object.
emitHhs :: CallItem -> Either String String
emitHhs ci = do
    let rs = ciRequestSpec ci
        url = case rqUrl rs of
            LexedUrlFull s -> s
            LexedUrlSegments ps -> concatMap partText ps
    unless ("http" `isPrefixOf` url) $
        Left ("the URL " ++ url ++ " doesn't start with http, which .hhs requires; run the snippet with --curl instead")
    let body = trim (rqBody rs)
    unless (null body || (take 1 body == "{" && take 1 (reverse body) == "}")) $
        Left "the body isn't a JSON object, which .hhs can't express; run the snippet with --curl instead"
    let RhsDict hs = rqHeaders rs
        headerLines = [ Text.unpack k ++ ": " ++ concatMap partText v | (k, v) <- sortOn fst (HM.toList hs) ]
        varLines = case sqVars (rqSquares rs) of
            Just (RequestSquareVars (RhsDict d)) | not (HM.null d) ->
                "[Vars]" : [ Text.unpack k ++ ": " ++ concatMap partText v | (k, v) <- sortOn fst (HM.toList d) ]
            _ -> []
    Right $ unlines $
        [ rqMethod rs ++ " " ++ url ]
        ++ headerLines
        ++ varLines
        ++ [ body | not (null body) ]
        ++ [ "HTTP 200" ]
  where
    partText (BEL.R t) = Text.unpack t
    partText (BEL.L t) = "{{" ++ Text.unpack t ++ "}}"
    trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse
