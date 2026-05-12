{
{-# LANGUAGE ScopedTypeVariables #-}
module P where

import Debug.Trace

import           Data.List (intercalate, isPrefixOf, inits, isSuffixOf)
import qualified Data.Text as Text
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HM
import           Control.Exception (try, SomeException)
import           Control.Monad.Trans.Except
import           Control.Monad.IO.Class (liftIO)
import           Network.HTTP.Types.Status (mkStatus)
import qualified Network.HTTP.Client as HC

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


script :: { E Script }
script : crlf call_items { $2 >>= \is -> returnE Script { kind = Regular, config = defaultScriptConfig, callItems = is } }

call_items :: { E [CallItem] }
call_items : call_item crlf            { $1 >>= \i -> returnE [i] }
           | call_items call_item crlf { $1 >>= \is -> $2 >>= \i -> returnE (is ++ [i]) }

call_item :: { E CallItem }
call_item : deps_clause request response { $2 >>= \r -> returnE CallItem { ciDeps = deps $1, ciName = itemName $1, ciRequestSpec = r, ciResponseSpec = Just $3 } }
          | deps_clause request          { $2 >>= \r -> returnE CallItem { ciDeps = deps $1, ciName = itemName $1, ciRequestSpec = r, ciResponseSpec = Nothing } }
          | request response             { $1 >>= \r -> returnE CallItem { ciDeps = [], ciName = "", ciRequestSpec = r, ciResponseSpec = Just $2 } }
          | request                      { $1 >>= \r -> returnE CallItem { ciDeps = [], ciName = "", ciRequestSpec = r, ciResponseSpec = Nothing } }

deps_clause :: { DepsClause }
deps_clause : deps "then" s { DepsClause { deps = $1, itemName = $3 } }
            | s             { DepsClause { deps = [], itemName = $1 } }

deps :: { [String] }
deps : s      { [$1] }
     | deps s { $1 ++ [$2] }

request :: { E RequestSpec }
request : method url crlf bindings request_sqrs braced crlf { do
                                                               let r = RequestSpec { rqMethod = $1,    rqUrl = $2, rqHeaders = $4,               rqSquares = $5,               rqBody = $6 }
                                                               trace "reqA" $ pure r }
        | method url crlf bindings request_sqrs        crlf { do
                                                               let r = RequestSpec { rqMethod = $1,    rqUrl = $2, rqHeaders = $4,               rqSquares = $5,               rqBody = "" }
                                                               trace "reqB" $ pure r }
        | method url crlf          request_sqrs        crlf { do
                                                               let r = RequestSpec { rqMethod = $1,    rqUrl = $2, rqHeaders = RhsDict HM.empty, rqSquares = $4,               rqBody = "" }
                                                               trace "reqB1" $ pure r }
        | method url crlf bindings              braced crlf { do
                                                               let r = RequestSpec { rqMethod = $1,    rqUrl = $2, rqHeaders = $4,               rqSquares = rqSquaresNothing, rqBody = $5 }
                                                               trace "reqC" $ pure r }
        | method url crlf          request_sqrs braced crlf { do
                                                               let r = RequestSpec { rqMethod = $1,    rqUrl = $2, rqHeaders = RhsDict HM.empty, rqSquares = $4,               rqBody = $5 }
                                                               trace "reqD" $ pure r }
        | method url               request_sqrs        crlf { do
                                                               let r = RequestSpec { rqMethod = $1,    rqUrl = $2, rqHeaders = RhsDict HM.empty, rqSquares = $3,               rqBody = "" }
                                                               trace "reqD1" $ pure r }
        | method url crlf                       braced crlf { do
                                                               let r = RequestSpec { rqMethod = $1,    rqUrl = $2, rqHeaders = RhsDict HM.empty, rqSquares = rqSquaresNothing, rqBody = $4 }
                                                               trace "reqE" $ pure r }
        | method url crlf bindings                     crlf { do
                                                               let r = RequestSpec { rqMethod = $1,    rqUrl = $2, rqHeaders = $4,               rqSquares = rqSquaresNothing, rqBody = "" }
                                                               trace "reqF" $ pure r }
        |        url crlf bindings request_sqrs        crlf { do
                                                               let r = RequestSpec { rqMethod = "GET", rqUrl = $1, rqHeaders = $3,               rqSquares = $4,               rqBody = "" }
                                                               trace "reqG" $ pure r }
        |        url crlf bindings              braced crlf { do
                                                               let r = RequestSpec { rqMethod = "GET", rqUrl = $1, rqHeaders = $3,               rqSquares = rqSquaresNothing, rqBody = $4 }
                                                               trace "reqH" $ pure r }
        |        url crlf          request_sqrs braced crlf { do
                                                               let r = RequestSpec { rqMethod = "GET", rqUrl = $1, rqHeaders = RhsDict HM.empty, rqSquares = $3,               rqBody = $4 }
                                                               trace "reqI" $ pure r }
        | method url                                   crlf { do
                                                               let r = RequestSpec { rqMethod = $1,    rqUrl = $2, rqHeaders = RhsDict HM.empty, rqSquares = rqSquaresNothing, rqBody = "" }
                                                               trace "reqJ" $ pure r }
        |        url crlf bindings                     crlf { do
                                                               let r = RequestSpec { rqMethod = "GET", rqUrl = $1, rqHeaders = $3,               rqSquares = rqSquaresNothing, rqBody = "" }
                                                               trace "reqK" $ pure r }
        |        url crlf          request_sqrs        crlf { do
                                                               let r = RequestSpec { rqMethod = "GET", rqUrl = $1, rqHeaders = RhsDict HM.empty, rqSquares = $3,               rqBody = "" }
                                                               trace "reqL" $ pure r }
        |        url crlf                       braced crlf { do
                                                               let r = RequestSpec { rqMethod = "GET", rqUrl = $1, rqHeaders = RhsDict HM.empty, rqSquares = rqSquaresNothing, rqBody = $3 }
                                                               trace "reqM" $ pure r }
        |        url crlf                                   { do
                                                               let r = RequestSpec { rqMethod = "GET", rqUrl = $1, rqHeaders = RhsDict HM.empty, rqSquares = rqSquaresNothing, rqBody = "" }
                                                               trace "reqN" $ pure r }

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
response : "HTTP" response_codes crlf bindings response_sqrs braced crlf { trace "rpA" $ ResponseSpec { rpStatuses = map statusFrom $2, rpResponseHeaders = $4,               rpSquares = $5,               rpBody = $6 } }

         | "HTTP" response_codes crlf bindings response_sqrs        crlf { trace "rpB" $ ResponseSpec { rpStatuses = map statusFrom $2, rpResponseHeaders = $4,               rpSquares = $5,               rpBody = "" } }
         | "HTTP" response_codes crlf bindings               braced crlf { trace "rpC" $ ResponseSpec { rpStatuses = map statusFrom $2, rpResponseHeaders = $4,               rpSquares = rpSquaresNothing, rpBody = $5 } }
         | "HTTP" response_codes crlf          response_sqrs braced crlf { trace "rpD" $ ResponseSpec { rpStatuses = map statusFrom $2, rpResponseHeaders = RhsDict HM.empty, rpSquares = $4,               rpBody = $5 } }
         |                            bindings response_sqrs braced crlf { trace "rpE" $ ResponseSpec { rpStatuses = [],                rpResponseHeaders = $1,               rpSquares = $2,               rpBody = $3 } }

         | "HTTP" response_codes crlf bindings                           { trace "rpF" $ ResponseSpec { rpStatuses = map statusFrom $2, rpResponseHeaders = $4,               rpSquares = rpSquaresNothing, rpBody = "" } }
         | "HTTP" response_codes crlf          response_sqrs        crlf { trace "rpG" $ ResponseSpec { rpStatuses = map statusFrom $2, rpResponseHeaders = RhsDict HM.empty, rpSquares = $4,               rpBody = "" } }
         | "HTTP" response_codes crlf                        braced crlf { trace "rpH" $ ResponseSpec { rpStatuses = map statusFrom $2, rpResponseHeaders = RhsDict HM.empty, rpSquares = rpSquaresNothing, rpBody = $4 } }
         |                            bindings response_sqrs        crlf { trace "rpI" $ ResponseSpec { rpStatuses = [],                rpResponseHeaders = $1,               rpSquares = $2,               rpBody = "" } }
         |                            bindings               braced crlf { trace "rpJ" $ ResponseSpec { rpStatuses = [],                rpResponseHeaders = $1,               rpSquares = rpSquaresNothing, rpBody = $2 } }
         |                                     response_sqrs braced crlf { trace "rpK" $ ResponseSpec { rpStatuses = [],                rpResponseHeaders = RhsDict HM.empty, rpSquares = $1,               rpBody = $2 } }

         | "HTTP" response_codes crlf                                    { trace "rpL" $ ResponseSpec { rpStatuses = map statusFrom $2, rpResponseHeaders = RhsDict HM.empty, rpSquares = rpSquaresNothing, rpBody = "" } }
         | bindings crlf                                                 { trace "rpM" $ ResponseSpec { rpStatuses = [],                rpResponseHeaders = $1, rpSquares = rpSquaresNothing,               rpBody = "" } }
         | response_sqrs crlf                                            { trace "rpN" $ ResponseSpec { rpStatuses = [],                rpResponseHeaders = RhsDict HM.empty, rpSquares = $1,               rpBody = "" } }
         | braced crlf                                                   { trace "rpO" $ ResponseSpec { rpStatuses = [],                rpResponseHeaders = RhsDict HM.empty, rpSquares = rpSquaresNothing, rpBody = $1 } }
         | crlf                                                          { trace "rpP" $ ResponseSpec { rpStatuses = [],                rpResponseHeaders = RhsDict HM.empty, rpSquares = rpSquaresNothing, rpBody = "" } }

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
                              RhsDict (HM.insertWith (++) (fst $2) (snd $2) acc) }

binding :: { Binding }
binding : identifier crlf     { (Text.pack $1, []) }
        | identifier s crlf   { (Text.pack $1, [BEL.R (Text.pack $2)]) }
        | identifier rhs crlf { (Text.pack $1, [BEL.L (Text.pack (drop 1 $2))]) }

crlf :: { () }
crlf : {- empty -}  { }
     | crlf newline { }


{

setResponseSquare sq (a, c) = case sq of
    ResponseSquareAsserts _ ->  (Just sq, c)
    ResponseSquareCaptures _ -> (a,       Just sq)
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
hasBalancedMustache s = countOpen > 0 && countOpen == countClose && countOpen == length (filter isOpenPair allPrefixes)
  where
    s' = filter (`elem` ['{', '}']) s
    countOpen = length $ filter isOpenPair allPrefixes
    countClose = length $ filter isClosePair allPrefixes
    allPrefixes = take (length s' - 1) (inits s')
    isOpenPair p = "{{" `isSuffixOf` p
    isClosePair p = "}}" `isSuffixOf` p
    inits [] = []
    inits xs = xs : inits (init xs)

-- statusFrom :: Int -> Status
statusFrom n = mkStatus n ""

stripColon :: String -> String
stripColon s = dropWhile (\c -> c == ':' || c == ' ') s

mergeRhsDicts :: RhsDict -> RhsDict -> RhsDict
mergeRhsDicts (RhsDict a) (RhsDict b) = RhsDict (HM.unionWith (++) a b)

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

catchE :: E a -> (String -> [Token] -> E a) -> E a
catchE m k = Control.Monad.Trans.Except.catchE m (\(e, ts) -> k e ts)
}
