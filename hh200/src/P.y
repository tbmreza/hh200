{
module P where

import L
}
%name parse
%tokentype { Token }
%error { parseError }
%token 
    d  { DIGITS _ $$ }
    s  { RAW _ $$ }

    "\n"       { LN _ }

    "."        { SYN _ }
    "/"        { SYN _ }
    "http"     { KW_HTTP _ }
    "HTTP"     { KW_HTTP _ }

    method     { METHOD _ $$ }

    scheme     { URL_SCHEME _ $$ }
    authority  { URL_AUTHORITY _ $$ }
    path       { URL_PATH _ $$ }
    query      { URL_QUERY _ $$ }
    fragment   { URL_AUTHORITY _ $$ }

%monad { E } { thenE } { returnE }

%%

E : Statements  { $1 }

Statements : Statement             { [$1] }
           | Statements Statement  { $1 ++ [$2] }

Statement  : method Expr            { RequestLine $1 $2 }

           | http_version_status { $1 }
           | request_line { $1 }


Expr       :  scheme authority paths { Url $1 $2 $3 Nothing Nothing }


request_line : method s "\n"  { Request { method = $1, url = "" } }

http_version_status : kwHttp sep d sep d d  { Response { version = Just (read ($3 ++ $5) :: Float), status = (read $6 :: Int) } }
                    | kwHttp sep d d        { Response { version = Just (read $3 :: Float), status = (read $4 :: Int) } }
                    | kwHttp d              { Response { version = Nothing, status = (read $2 :: Int) } }

kwHttp : "http" { "http" }
       | "HTTP" { "HTTP" }

sep : "/" { "/" }
    | "." { "." }


paths : path paths      { $1 : $2 }
      | {- empty -}     { [] }

queries : query         { Just $1 }
        | {- empty -}   { Nothing }

fragments : fragment    { Just $1 }
          | {- empty -} { Nothing }


{

data Statement = RequestLine  String Expr
               | Request { method :: String, url :: String }
               | Response { version :: Maybe Float, status :: Int }
    deriving (Show)

data Expr = IntLit  Int
          | Url     String String [String] (Maybe String) (Maybe String)
    deriving (Show)


parseError :: [Token] -> E a
parseError tokens = failE "Parse error"

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
