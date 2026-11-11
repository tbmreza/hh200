{
{-# LANGUAGE ScopedTypeVariables #-}
module P where

import           Data.List (intercalate, isPrefixOf, tails)
import           Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import qualified Data.HashMap.Strict as HM
import           Control.Monad.Trans.Except
import           Network.HTTP.Types.Status (Status, mkStatus)

import qualified BEL
import           Hh200.Types
import           L

}

%name parse script
%tokentype { Token }
%error { parseError }
%token
    d           { DIGITS _ $$ }

    identifier  { IDENTIFIER _ $$ }


    newline     { LN _ }

    "="         { SEP _ }
    "/"         { SEP _ }
    "."         { SEP _ }
    ","         { SEP _ }
    ":"         { COLON _ }
    "\""        { QUOTE _ }
    "{"         { BRACE_OPN _ }
    "}"         { BRACE_CLS _ }
    "("         { PAREN_OPN _ }
    ")"         { PAREN_CLS _ }
    "["         { LIST_OPN _ }
    "]"         { LIST_CLS _ }
    "then"      { KW_THEN _ }
    "HTTP"      { KW_HTTP _ }

    "Configs"           { KW_CONFIGS _ }
    "Query"             { KW_QUERY _ }
    "FormParams"        { KW_FORM _ }
    "Form"              { KW_FORM _ }
    "MultipartFormData" { KW_MULTIPART _ }
    "Cookies"           { KW_COOKIES _ }

    "Captures"  { KW_CAPTURES _ }
    "Asserts"   { KW_ASSERTS _ }

    method      { METHOD _ $$ }

    url_proto   { URL _ $$ }
    s           { QUOTED _ $$ }
    braced      { BRACE_ENCLOSED _ $$ }
    rhs         { RHS _ $$ }


    line        { LINE _ $$ }


%monad { E } { thenE } { returnE }

%%


script :: { Script }
script : crlf call_items { Script { config = defaultScriptConfig, callItems = $2, index = emptyDocIndex } }

call_items :: { [CallItem] }
call_items : call_item crlf            { [$1] }
           | call_items call_item crlf { $1 ++ [$2] }

call_item :: { CallItem }
call_item : deps_clause request response { CallItem { ciDeps = deps $1, ciName = itemName $1, ciRequestSpec = $2, ciResponseSpec = Just $3 } }
          | deps_clause request          { CallItem { ciDeps = deps $1, ciName = itemName $1, ciRequestSpec = $2, ciResponseSpec = Nothing } }
          | request response             { CallItem { ciDeps = [], ciName = "", ciRequestSpec = $1, ciResponseSpec = Just $2 } }
          | request                      { CallItem { ciDeps = [], ciName = "", ciRequestSpec = $1, ciResponseSpec = Nothing } }

deps_clause :: { DepsClause }
deps_clause : deps "then" s { DepsClause { deps = $1, itemName = trimQuotes $3 } }
            | s             { DepsClause { deps = [], itemName = trimQuotes $1 } }

deps :: { [String] }
deps : s      { [trimQuotes $1] }
     | deps s { $1 ++ [trimQuotes $2] }

-- The @opt_*@ productions make the trailing parts of a request/response
-- optional. Combined with the empty-matchable 'crlf', this yields shift/reduce
-- conflicts (Happy resolves them by shifting, which greedily consumes newlines
-- and parses each optional part correctly); they are benign.
request :: { RequestSpec }
request : method url crlf opt_bindings opt_sqrs opt_body crlf { mkRequest $1 $2 $4 $5 $6 }
        |        url crlf opt_bindings opt_sqrs opt_body crlf { mkRequest "GET" $1 $3 $4 $5 }

opt_bindings :: { Maybe RhsDict }
opt_bindings : {- empty -} { Nothing }
             | bindings    { Just $1 }

opt_sqrs :: { (Maybe RequestSquare, Maybe RequestSquare, Maybe RequestSquare, Maybe RequestSquare, Maybe RequestSquare) }
opt_sqrs : {- empty -}  { rqSquaresNothing }
         | request_sqrs { $1 }

opt_body :: { Maybe String }
opt_body : {- empty -} { Nothing }
         | braced      { Just $1 }

request_sqrs :: { (Maybe RequestSquare, Maybe RequestSquare, Maybe RequestSquare, Maybe RequestSquare, Maybe RequestSquare) }
request_sqrs : request_sqrs request_sqr crlf { setRequestSquare $2 $1 }
             | request_sqr crlf              { initRequestSquare $1 }

request_sqr :: { RequestSquare }
request_sqr : "[" "Configs" "]" crlf bindings           { RequestSquareConfigs $5 }
            | "[" "Query" "]" crlf bindings             { RequestSquareQuery $5 }
            | "[" "FormParams" "]" crlf bindings        { RequestSquareForm $5 }
            | "[" "Form" "]" crlf bindings              { RequestSquareForm $5 }
            | "[" "MultipartFormData" "]" crlf bindings { RequestSquareMultipart $5 }
            | "[" "Cookies" "]" crlf bindings           { RequestSquareCookies $5 }

url :: { LexedUrl }
url : url_proto { if hasBalancedMustache $1 then
                      LexedUrlSegments (BEL.partitions (Text.pack $1))
                  else
                      LexedUrlFull $1 }

response :: { ResponseSpec }
response : response_parts crlf { mkResponse $1 }

response_parts :: { ([Status], Maybe RhsDict, (Maybe ResponseSquare, Maybe ResponseSquare), Maybe String) }
response_parts : "HTTP" response_codes crlf opt_bindings opt_rsqrs opt_body { (map statusFrom $2, $4, $5, $6) }
               | bindings opt_rsqrs opt_body                              { ([], Just $1, $2, $3) }
               | response_sqrs opt_body                                   { ([], Nothing, $1, $2) }
               | braced                                                   { ([], Nothing, rpSquaresNothing, Just $1) }

opt_rsqrs :: { (Maybe ResponseSquare, Maybe ResponseSquare) }
opt_rsqrs : {- empty -}   { rpSquaresNothing }
          | response_sqrs { $1 }

response_sqrs :: { (Maybe ResponseSquare, Maybe ResponseSquare)}
response_sqrs : response_sqrs response_sqr crlf { setResponseSquare $2 $1 }
              | response_sqr crlf               { initResponseSquare $1 }

response_sqr :: { ResponseSquare }
response_sqr : "[" "Captures" "]" crlf bindings { ResponseSquareCaptures $5 }
             | response_asserts                 { ResponseSquareAsserts $1 }

response_asserts :: { [String] }
response_asserts : "[" "Asserts" "]" crlf expr_lines { $5 }
                 | "[" "Asserts" "]" crlf            { [] }

expr_lines :: { [String] }
expr_lines : line crlf            { [$1] }
           | expr_lines line crlf { ($1 ++ [$2]) }

response_codes :: { [Int] }
response_codes : d                { [read $1] }
               | response_codes d { $1 ++ [read $2] }

bindings :: { RhsDict }
bindings : binding          { RhsDict (HM.fromList [$1]) }
         | bindings binding { let (RhsDict acc) = $1 in
                              RhsDict (HM.insertWith (flip (++)) (fst $2) (snd $2) acc) }

binding :: { Binding }
binding : identifier crlf     { (Text.pack $1, []) }
        | identifier s crlf   { (Text.pack $1, [BEL.R (Text.pack (trimQuotes $2))]) }
        | identifier rhs crlf { (Text.pack $1, BEL.partitions (Text.pack (stripRhs $2))) }

crlf :: { () }
crlf : {- empty -}  { }
     | crlf newline { }


{

setResponseSquare sq (a, c) = case sq of
    ResponseSquareCaptures _ -> (Just sq, c)
    ResponseSquareAsserts _ ->  (a,       Just sq)
    _                      ->   (a,       c)

initResponseSquare sq = case sq of
    ResponseSquareCaptures _ -> (Just sq, Nothing)
    ResponseSquareAsserts _ ->  (Nothing, Just sq)

setRequestSquare sq (c, q, f, m, k) = case sq of
    RequestSquareConfigs _ ->   (Just sq, q,       f,       m,       k)
    RequestSquareQuery _ ->     (c,       Just sq, f,       m,       k)
    RequestSquareForm _ ->      (c,       q,       Just sq, m,       k)
    RequestSquareMultipart _ -> (c,       q,       f,       Just sq, k)
    RequestSquareCookies _ ->   (c,       q,       f,       m,       Just sq)
    _                      ->   (c,       q,       f,       m,       k)

initRequestSquare sq = case sq of
    RequestSquareConfigs _ ->   (Just sq, Nothing, Nothing, Nothing, Nothing)
    RequestSquareQuery _ ->     (Nothing, Just sq, Nothing, Nothing, Nothing)
    RequestSquareForm _ ->      (Nothing, Nothing, Just sq, Nothing, Nothing)
    RequestSquareMultipart _ -> (Nothing, Nothing, Nothing, Just sq, Nothing)
    RequestSquareCookies _ ->   (Nothing, Nothing, Nothing, Nothing, Just sq)

hasBalancedMustache :: String -> Bool
hasBalancedMustache s = countOpen > 0 && countOpen == countClose
  where
    braces     = filter (`elem` ['{', '}']) s
    countOpen  = countSub "{{" braces
    countClose = countSub "}}" braces

countSub :: String -> String -> Int
countSub needle = length . filter (isPrefixOf needle) . tails

-- statusFrom :: Int -> Status
statusFrom n = mkStatus n ""

mkRequest :: String -> LexedUrl -> Maybe RhsDict -> (Maybe RequestSquare, Maybe RequestSquare, Maybe RequestSquare, Maybe RequestSquare, Maybe RequestSquare) -> Maybe String -> RequestSpec
mkRequest method url mHeaders squares mBody =
    RequestSpec
        { rqMethod = method
        , rqUrl = url
        , rqHeaders = fromMaybe (RhsDict HM.empty) mHeaders
        , rqSquares = squares
        , rqBody = fromMaybe "" mBody
        }

mkResponse :: ([Status], Maybe RhsDict, (Maybe ResponseSquare, Maybe ResponseSquare), Maybe String) -> ResponseSpec
mkResponse (statuses, mHeaders, squares, mBody) =
    ResponseSpec
        { rpStatuses = statuses
        , rpResponseHeaders = fromMaybe (RhsDict HM.empty) mHeaders
        , rpSquares = squares
        , rpBody = fromMaybe "" mBody
        }

-- Strip a leading ':' plus surrounding whitespace, then optional surrounding
-- double quotes, leaving the bare header/field value.
stripRhs :: String -> String
stripRhs = trimQuotes . reverse . dropWhile (== ' ') . reverse . dropWhile (\c -> c == ':' || c == ' ')

parseError :: [Token] -> E a
parseError tokens = failE ("Parse error on tokens:\n" ++ prettyTokens tokens) tokens

prettyTokens :: [Token] -> String
prettyTokens = intercalate "\n" . map (("  " ++) . prettyToken) . take 10

prettyToken :: Token -> String
prettyToken t = case t of
    LN p           -> "LN " ++ showPos p
    DIGITS p s     -> "DIGITS " ++ showPos p ++ " " ++ show s
    IDENTIFIER p s -> "IDENTIFIER " ++ showPos p ++ " " ++ show s
    SEP p          -> "SEP " ++ showPos p
    METHOD p s     -> "METHOD " ++ showPos p ++ " " ++ show s
    VERSION p s    -> "VERSION " ++ showPos p ++ " " ++ show s
    BRACE_OPN p    -> "BRACE_OPN " ++ showPos p
    BRACE_CLS p    -> "BRACE_CLS " ++ showPos p
    PAREN_OPN p    -> "PAREN_OPN " ++ showPos p
    PAREN_CLS p    -> "PAREN_CLS " ++ showPos p
    LIST_OPN p     -> "LIST_OPN " ++ showPos p
    LIST_CLS p     -> "LIST_CLS " ++ showPos p
    COLON p        -> "COLON " ++ showPos p
    QUOTE p        -> "QUOTE " ++ showPos p
    KW_THEN p      -> "KW_THEN " ++ showPos p
    KW_HTTP p      -> "KW_HTTP " ++ showPos p
    KW_CONFIGS p   -> "KW_CONFIGS " ++ showPos p
    KW_CAPTURES p  -> "KW_CAPTURES " ++ showPos p
    KW_ASSERTS p   -> "KW_ASSERTS " ++ showPos p
    URL p s        -> "URL " ++ showPos p ++ " " ++ show s
    QUOTED p s     -> "QUOTED " ++ showPos p ++ " " ++ show s
    BRACED p s     -> "BRACED " ++ showPos p ++ " " ++ show s
    BRACE_ENCLOSED p s -> "BRACE_ENCLOSED " ++ showPos p ++ " " ++ show s
    RHS p s        -> "RHS " ++ showPos p ++ " " ++ show s
    JSONPATH p s   -> "JSONPATH " ++ showPos p ++ " " ++ show s
    LINE p s       -> "LINE " ++ showPos p ++ " " ++ show s
    KW_COOKIES p -> "KW_COOKIES " ++ showPos p
    KW_QUERY p -> "KW_QUERY " ++ showPos p
    KW_FORM p -> "KW_FORM " ++ showPos p
    KW_MULTIPART p -> "KW_MULTIPART " ++ showPos p
    EOF p          -> "EOF " ++ showPos p

showPos :: AlexPosn -> String
showPos (AlexPn _ l c) = "(" ++ show l ++ ":" ++ show c ++ ")"


type E a = ExceptT (String, [Token]) IO a

thenE :: E a -> (a -> E b) -> E b
thenE = (>>=)

returnE :: a -> E a
returnE = return

failE :: String -> [Token] -> E a
failE err tokens = throwE (err, tokens)
}
