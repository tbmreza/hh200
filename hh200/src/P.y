{
-- Export current module with L addition.
-- module P ( module P, module L ) where
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

    method      { METHOD _ $$ }

    url         { URL _ $$ }
    s           { QUOTED _ $$ }
    jsonBody    { BRACED _ $$ }


%monad { E } { thenE } { returnE }

%%

-- test_suite : directives call_items
--            | directives
--            | call_items

script : crlf call_items crlf    { Script { config = defaultScriptConfig, callItems = $2 } }

crlf : {- optional newline -} { }
     | crlf newline           { }

deps_clause : deps "then" s { DepsClause { deps = $1, itemName = $3 } }
            | s             { DepsClause { deps = [], itemName = $1 } }

deps : s      { [$1] }
     | deps s { $1 ++ [$2] }

request  : method url crlf jsonBody crlf { trace "traceA" RequestSpec { verb = expectUpper    $1, url = $2, headers = [], payload = $4, opts = [] } }
         | method url crlf               { trace "traceB" RequestSpec { verb = expectUpper    $1, url = $2, headers = [], payload = "", opts = [] } }
         | url crlf                      { trace "traceC" RequestSpec { verb = expectUpper "GET", url = $1, headers = [], payload = "", opts = [] } }

response : "HTTP" response_codes { ResponseSpec { output = [], statuses = [] } }

response_codes : "[" status_list "]" { $2 }
               | status_list { $1 }

status_list :: { [Int] }
status_list : status      { [$1] }
            | status_list status { $1 ++ [$2] }

call_item : deps_clause request response { pCallItem $1 $2 (Just $3) }
          | deps_clause request          { pCallItem $1 $2 Nothing }
          | request                      { pCallItem defaultDepsClause $1 Nothing }
          | request response             { pCallItem defaultDepsClause $1 (Just $2) }


call_items : call_item crlf           { [$1] }
           | call_items call_item  { $1 ++ [$2] }

{

-- getLineNum :: Token -> Int
-- getLineNum (Token (AlexPn _ line _) _) = line
-- traceWithLine :: Token -> String -> a -> a
-- traceWithLine tok msg val = trace (msg ++ " at line " ++ show (getLineNum tok)) val


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
