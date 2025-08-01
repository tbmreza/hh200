{
-- Export current module with L addition.
-- module P ( module P, module L ) where
module P where

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
    identifier  { IDENTIFIER _ $$ }
    filepath    { PATH _ $$ }

    "\n"        { LN _ }

    "/"         { SEP _ }
    "."         { SEP _ }
    ","         { SEP _ }
    ":"         { COLON _ }
    "\""        { QUOTE _ }
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


%monad { E } { thenE } { returnE }

%%

testsuite : callables           { Script { config = defaultScriptConfig, call_items = $1 } }
          | directive callables { Script { config = $1, call_items = $2 } }

directive : "then"  { defaultScriptConfig }

callables : callable  { [$1] }
          | callables callable  { $1 ++ [$2] }

callable
    : method url
    { CallItem
        { ci_deps = []
        , ci_name = $1
        , ci_request_spec = RequestSpec { url = $2, verb = BS.pack $1 }
        , ci_response_spec = Nothing
        } }

    | url
    { CallItem
        { ci_deps = []
        , ci_name = $1
        , ci_request_spec = RequestSpec { url = $1, verb = "GET" }
        , ci_response_spec = Nothing
        } }

{

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
