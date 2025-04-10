{
module P where

import Control.Monad.Trans.Except
import L
}
%name parse
%tokentype { Token }
%error { parseError }
%token
    s           { RAW _ $$ }
    d           { DIGITS _ $$ }
    identifier  { IDENTIFIER _ $$ }
    filepath    { PATH _ $$ }

    "\n"       { LN _ }

    "/"        { SEP _ }
    "."        { SEP _ }
    ","        { SEP _ }
    ":"        { COLON _ }
    "["        { LIST_OPN _ }
    "]"        { LIST_CLS _ }
    "http"     { KW_HTTP _ }
    "HTTP"     { KW_HTTP _ }
    "Config"   { KW_CONFIG _ }

    method     { METHOD _ $$ }

    scheme     { URL_SCHEME _ $$ }
    authority  { URL_AUTHORITY _ $$ }
    path       { URL_PATH _ $$ }
    query      { URL_QUERY _ $$ }
    fragment   { URL_AUTHORITY _ $$ }

%monad { E } { thenE } { returnE }

%%

Program : Statements  { $1 }

Statements : Statement             { [$1] }
           | Statements Statement  { $1 ++ [$2] }

Statement  : http_version_status { $1 }
           | request_line { $1 }
           | config_section { $1 }
           | sect { $1 }



http_version_status : kwHttp "/" d "." d status_codes  { Response { version = Just (read ($3 ++ $5) :: Float), status = $6 } }
                    | kwHttp "/" d       status_codes  { Response { version = Just (read $3 :: Float), status = $4 } }
                    | kwHttp             status_codes  { Response { version = Nothing, status = $2 } }

request_line : method s "\n"  { Request { method = $1, url = $2 } }
             | method s       { Request { method = $1, url = $2 } }

sect : "[" kwConfig "]"  { Section { name = $2, binds = [] } }

config_section : "[" kwConfig "]" "\n"
    identifier ":" filepath "\n"
    identifier ":" identifier
                    { Config { output = $7, outputExists = $11 } }

kwHttp : "http" { "http" }
       | "HTTP" { "HTTP" }

kwConfig : "Config" { "Config" }



paths :: { [String] }
paths : path paths      { $1 : $2 }
      | {- empty -}     { [] }

queries :: { Maybe String }
queries : query         { Just $1 }
        | {- empty -}   { Nothing }

fragments :: { Maybe String }
fragments : fragment    { Just $1 }
          | {- empty -} { Nothing }

status_codes :: { [Int] }
status_codes : "[" numbers "]"  { $2 }
             | numbers          { $1 }

numbers :: { [Int] }
numbers : d          { [read $1] }
        | numbers d  { $1 ++ [read $2] }

{

data Statement = Request { method :: String, url :: String }
               | Response { version :: Maybe Float, status :: [Int] }
               | Section { name :: String, binds :: [String] }

               | Config { output :: String, outputExists :: String }
-- Statement::Config (section name not a free text)
-- [Config] output: /home/tbmreza/test.jpg \n output-exists: overwrite  # overwrite | warn | error | fresh
    deriving (Show, Eq)


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
