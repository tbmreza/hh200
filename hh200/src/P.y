{
module P where

import Debug.Trace

import           Data.List (intercalate)
import qualified Data.Text as Text
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HM
import           Control.Monad.Trans.Except
import           Control.Monad.IO.Class (liftIO)
-- import           Network.HTTP.Types.Status (Status, mkStatus)
import           Network.HTTP.Types.Status (mkStatus)
import qualified Network.HTTP.Client as HC

import qualified BEL
import           Hh200.Types
import           L

}
%name parse
%name parseg scriptg
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
    "Configs"   { KW_CONFIGS _ }
    "Cookies"   { KW_COOKIES _ }
    "Captures"  { KW_CAPTURES _ }
    "Asserts"   { KW_ASSERTS _ }

    method      { METHOD _ $$ }

    url         { URL _ $$ }
    s           { QUOTED _ $$ }
    braced      { BRACE_ENCLOSED _ $$ }
    braced_old  { BRACED _ $$ }
    rhs         { RHS _ $$ }


    line        { LINE _ $$ }


%monad { E } { thenE } { returnE }

%%

script : crlf call_items  { trace "root1" $ Script { kind = Regular, config = defaultScriptConfig, callItems = $2 } }

scriptg :: { E [CallItemg] }
scriptg : crlf call_itemsg { $2 }

crlf : {- optional newline -} { }
     | crlf newline           { }

request_configs :: { RhsDict }
request_configs : "[" "Configs" "]" crlf bindings { $5 }

request_cookies :: { RhsDict }
request_cookies : "[" "Cookies" "]" crlf bindings { $5 }



deps_clause : deps "then" s { DepsClause { deps = $1, itemName = $3 } }
            | s             { DepsClause { deps = [], itemName = $1 } }

deps : s      { [$1] }
     | deps s { $1 ++ [$2] }

request : method url crlf bindings request_configs braced crlf { trace "rq0" $ RequestSpec { verb = expectUpper    $1, url = expectUrl $2, headers = $4, payload = $6, configs = $5 } }
        | method url crlf bindings                 braced crlf { trace "rq1" $ RequestSpec { verb = expectUpper    $1, url = expectUrl $2, headers = $4, payload = $5, configs = RhsDict HM.empty } }
        | method url crlf bindings                        crlf { trace "rq2" $ RequestSpec { verb = expectUpper    $1, url = expectUrl $2, headers = $4, payload = "", configs = RhsDict HM.empty } }
        | method url crlf                          braced crlf { trace "rq3" $ RequestSpec { verb = expectUpper    $1, url = expectUrl $2, headers = RhsDict HM.empty, payload = $4, configs = RhsDict HM.empty } }
        | method url crlf                                      { trace "rq4" $ RequestSpec { verb = expectUpper    $1, url = expectUrl $2, headers = RhsDict HM.empty, payload = "", configs = RhsDict HM.empty } }
        |        url crlf                                      { trace "rq5" $ RequestSpec { verb = expectUpper "GET", url = expectUrl $1, headers = RhsDict HM.empty, payload = "", configs = RhsDict HM.empty } }


requesg :: { E RequestSpeg }
requesg : method url crlf bindings request_configs braced crlf
        { do
            req <- liftIO $ HC.parseRequest $2
            returnE $ RequestSpeg { requestStruct = Just (req { HC.method = BS.pack $1 }) }
        }
        | method url crlf
        { do
            req <- liftIO $ HC.parseRequest $2
            returnE $ RequestSpeg { requestStruct = Just (req { HC.method = BS.pack $1 }) }
        }
        | url crlf
        { do
            req <- liftIO $ HC.parseRequest $1
            returnE $ RequestSpeg { requestStruct = Just req }
        }

response : "HTTP" response_codes crlf response_captures crlf response_asserts  crlf { trace "RSa" $ ResponseSpec { asserts = $6, captures = $4, output = [], statuses = map statusFrom $2 } }
         | "HTTP" response_codes crlf response_asserts  crlf response_captures crlf { trace "RSa_inv" $ ResponseSpec { asserts = $4, captures = $6, output = [], statuses = map statusFrom $2 } }
         | "HTTP" response_codes crlf                        response_captures crlf { trace "RSb" $ ResponseSpec { asserts = [], captures = $4, output = [], statuses = map statusFrom $2 } }
         | "HTTP" response_codes crlf response_asserts  crlf                        { trace "RSc" $ ResponseSpec { asserts = $4, captures = RhsDict HM.empty, output = [], statuses = map statusFrom $2 } }
         | "HTTP" response_codes crlf                                               { trace "RSd" $ ResponseSpec { asserts = [], captures = RhsDict HM.empty, output = [], statuses = map statusFrom $2 } }
         |                            response_captures crlf response_asserts  crlf { trace "RSe" $ ResponseSpec { asserts = $3, captures = $1, output = [], statuses = [] } }
         |                            response_asserts  crlf response_captures crlf { trace "RSe_inv" $ ResponseSpec { asserts = $1, captures = $3, output = [], statuses = [] } }
         |                            response_captures crlf                        { trace "RSf" $ ResponseSpec { asserts = [], captures = $1, output = [], statuses = [] } }
         |                            response_asserts  crlf                        { trace "RSg" $ ResponseSpec { asserts = $1, captures = RhsDict HM.empty, output = [], statuses = [] } }

response_captures :: { RhsDict }
response_captures : "[" "Captures" "]" crlf bindings { $5 }


bindings :: { RhsDict }
bindings : binding          { RhsDict (HM.fromList [$1]) }
         | bindings binding { let (RhsDict acc) = $1 in
                              RhsDict (HM.insertWith (++) (fst $2) (snd $2) acc) }

binding :: { Binding }
binding : identifier ":" s crlf  { ($1, [BEL.R (Text.pack $3)]) }
        | identifier rhs crlf    { ($1, [BEL.L (Text.pack (drop 1 $2))]) }


response_asserts :: { [String] }
response_asserts : "[" "Asserts" "]" crlf expr_lines { $5 }

expr_lines : line crlf       { trace ("line:" ++ show $1) $ [$1] }
           | expr_lines line { ($1 ++ [$2]) }

response_codes :: { [Int] }
response_codes : d                { [read $1] }
               | response_codes d { $1 ++ [read $2] }


call_item : deps_clause request response { pCallItem $1 $2 (Just $3) }
          | deps_clause request          { pCallItem $1 $2 Nothing }
          | request response             { pCallItem defaultDepsClause $1 (Just $2) }
          | request                      { pCallItem defaultDepsClause $1 Nothing } 


call_itemg :: { E CallItemg }
call_itemg : deps_clause requesg response { $2 >>= \r -> returnE (gCallItem $1 r (Just $3)) }
          | deps_clause requesg          { $2 >>= \r -> returnE (gCallItem $1 r Nothing) }
          | requesg response             { $1 >>= \r -> returnE (gCallItem defaultDepsClause r (Just $2)) }
          | requesg                      { $1 >>= \r -> returnE (gCallItem defaultDepsClause r Nothing) }



call_items : call_item crlf            { [$1] }
           | call_items call_item crlf { $1 ++ [$2] }

call_itemsg :: { E [CallItemg] }
call_itemsg : call_itemg crlf            { $1 >>= \i -> returnE [i] }
           | call_itemsg call_itemg crlf { $1 >>= \is -> $2 >>= \i -> returnE (is ++ [i]) }

{

-- statusFrom :: Int -> Status
statusFrom n = mkStatus n ""

stripColon :: String -> String
stripColon s = dropWhile (\c -> c == ':' || c == ' ') s

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
