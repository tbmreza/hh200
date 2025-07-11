{
-- Export current module with L addition.
-- module P ( module P, module L ) where
module P where

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

Program : Callables  { $1 }

Callables : Callable            { [$1] }
          | Callables Callable  { $1 ++ [$2] }

Callable : deps "then" dep request_ln response_ln
     { Callable
         { deps = $1
         , name = $3
         , request_spec = $4
         , response_spec = $5
         , been_called = False
         , err_stack = []
         }
     }

dep : s "\n"  { $1 }
    | s       { $1 }

deps :: { [String] }
deps : dep       { [$1] }
     | deps dep  { $1 ++ [$2] }

request_ln : method url "\n"  { RequestSpec { method = $1, url = $2, headers = [], payload = "", opts = [] } }
           | method url       { RequestSpec { method = $1, url = $2, headers = [], payload = "", opts = [] } }

response_ln : http_version_status config_output "\n"  { ResponseSpec { codes = $1, output = $2 } }
            | http_version_status config_output       { ResponseSpec { codes = $1, output = $2 } }

            | http_version_status "\n"  { ResponseSpec { codes = $1, output = []} }
            | http_version_status       { ResponseSpec { codes = $1, output = [] } }

config_output :: { [String] }
config_output : "(" s identifier ")" "\n"  { [$2, $3] }
              | "(" s identifier ")"       { [$2, $3] }

              | "(" s ")" "\n"  { [$2] }
              | "(" s ")"       { [$2] }

              | "(" ")" "\n"  { [] }
              | "(" ")"       { [] }

http_version_status : kwHttp "/" d "." d status_codes  { $6 }
                    | kwHttp "/" d       status_codes  { $4 }
                    | kwHttp             status_codes  { $2 }

kwHttp : "http" { "http" }
       | "HTTP" { "HTTP" }

status_codes :: { [Int] }
status_codes : "[" numbers "]"  { $2 }
             | numbers          { $1 }

numbers :: { [Int] }
numbers : d          { [read $1] }
        | numbers d  { $1 ++ [read $2] }

{

-- HTTP [200 201] ("/home/tbmreza/test.jpg" overwrite)
-- HTTP [200 201] ("/home/tbmreza/test.jpg")
-- HTTP 200 ("/home/tbmreza/test.jpg")
-- ??: when to interpret Config section
-- "login" then "checkin"
-- GET https://fastly.picsum.photos/id/19/200/200.jpg?hmac=U8dBrPCcPP89QG1EanVOKG3qBsZwAvtCLUrfeXdE0FI

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
