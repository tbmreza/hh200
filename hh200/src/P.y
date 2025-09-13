{
module P where

import Debug.Trace
import qualified Data.ByteString.Char8 as BS
import Control.Monad.Trans.Except
import L
import Hh200.Types
}
%name parse
%tokentype { Token }
%error { parseError }
%token
    d           { DIGITS _ $$ }
    status      { STATUS _ $$ }
    identifier  { IDENTIFIER _ $$ }


    newline        { LN _ }

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
    "http"      { KW_HTTP _ }
    "HTTP"      { KW_HTTP _ }
    "Config"    { KW_CONFIG _ }
    "Captures"  { KW_CAPTURES _ }

    method      { METHOD _ $$ }
    header      { HEADER _ $$ }

    url         { URL _ $$ }
    s           { QUOTED _ $$ }
    jsonBody    { BRACED _ $$ }

    jsonpath    { JSONPATH _ $$ }
    headerVal   { HEADER_VAL _ $$ }


%monad { E } { thenE } { returnE }

%%

script : crlf call_items crlf    { Script { config = defaultScriptConfig, callItems = $2 } }

crlf : {- optional newline -} { }
     | crlf newline           { }

deps_clause : deps "then" s { DepsClause { deps = $1, itemName = $3 } }
            | s             { DepsClause { deps = [], itemName = $1 } }

deps : s      { [$1] }
     | deps s { $1 ++ [$2] }

request_headers :: { [Binding] }
request_headers : request_header                 { [$1] }
                | request_headers request_header { ($1 ++ [$2]) }

request_header : header ":" headerVal crlf { ($1, $3) }

request  : method url crlf request_headers jsonBody crlf { trace "requestA" RequestSpec { verb = expectUpper    $1, url = $2, headers = $4, payload = $5, opts = [] } }
         | method url crlf request_headers crlf          { trace "requestB" RequestSpec { verb = expectUpper    $1, url = $2, headers = $4, payload = "", opts = [] } }
         | method url crlf jsonBody crlf                 { trace "requestC" RequestSpec { verb = expectUpper    $1, url = $2, headers = [], payload = $4, opts = [] } }
         | method url crlf                               { trace "requestD" RequestSpec { verb = expectUpper    $1, url = $2, headers = [], payload = "", opts = [] } }
         | url crlf                                      { trace "requestE" RequestSpec { verb = expectUpper "GET", url = $1, headers = [], payload = "", opts = [] } }

response : response_codes crlf response_captures { trace "responseA" (ResponseSpec { captures = mkCaptures $3, output = [], statuses = map statusFrom $1 }) }
         | response_codes                        { trace "responseB" (ResponseSpec { captures = mtCaptures, output = [], statuses = map statusFrom $1 }) }
         | response_captures                     { trace "responseC" (ResponseSpec { captures = mkCaptures $1, output = [], statuses = [] }) }

response_captures :: { [Binding] }
response_captures : "[" "Captures" "]" crlf bindings { $5 }

bindings :: { [Binding] }
bindings : binding          { trace "bindingsA" [$1] }
         | bindings binding { trace "bindingsB" ($1 ++ [$2]) }

binding : identifier "=" jsonpath crlf  { trace "bindingA" ($1, $3) }

response_codes :: { [Int] }
response_codes : d                { [read $1] }
               | response_codes d { $1 ++ [read $2] }


call_item : deps_clause request response { trace "call_itemA" (pCallItem $1 $2 (Just $3)) }
          | deps_clause request          { trace "call_itemB" (pCallItem $1 $2 Nothing) }
          | request response             { trace ("call_itemC: " ++ show $2) pCallItem defaultDepsClause $1 (Just $2) }
          | request                      { trace "call_itemD" (pCallItem defaultDepsClause $1 Nothing) } 


call_items : call_item crlf           { [$1] }
           | call_items call_item  { $1 ++ [$2] }

{

-- getLineNum :: Token -> Int
-- getLineNum (Token (AlexPn _ line _) _) = line
-- traceWithLine :: Token -> String -> a -> a
-- traceWithLine tok msg val = trace (msg ++ " at line " ++ show (getLineNum tok)) val

statusFrom :: Int -> Status
statusFrom n = mkStatus n ""


parseError :: [Token] -> E a
parseError tokens = failE $ "Parse error on tokens: " ++ show tokens

data E a = ParseOk a | ParseFailed String

thenE :: E a -> (a -> E b) -> E b
m `thenE` k =
   case m of
       ParseOk a     -> k a
       ParseFailed e -> ParseFailed e

returnE :: a -> E a
returnE a = ParseOk a

failE :: String -> E a
failE err = ParseFailed err

catchE :: E a -> (String -> E a) -> E a
catchE m k =
   case m of
      ParseOk a     -> ParseOk a
      ParseFailed e -> k e
}
