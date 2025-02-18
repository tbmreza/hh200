{
module P where

import L
}
%name parse
%tokentype { Token }
%error { parseError }
%token 
    httpUpper  { SKIP $$ }

    method     { METHOD _ $$ }

    scheme     { URL_SCHEME _ $$ }
    authority  { URL_AUTHORITY _ $$ }
    path       { URL_PATH _ $$ }
    query      { URL_QUERY _ $$ }
    fragment   { URL_AUTHORITY _ $$ }

%%

Program    : Statements             { $1 }

Statements : Statement              { [$1] }
           | Statements Statement   { $1 ++ [$2] }

Statement  : httpUpper Expr         { Response $2 }
           | method Expr            { RequestLine $1 $2 }

Expr       : httpUpper              { IntLit 1 }
           | scheme authority paths { Url $1 $2 $3 Nothing Nothing }


paths : path paths      { $1 : $2 }
      | {- empty -}     { [] }

queries : query         { Just $1 }
        | {- empty -}   { Nothing }

fragments : fragment    { Just $1 }
          | {- empty -} { Nothing }

{
data Statement = Response     Expr
               | RequestLine  String Expr
    deriving (Show)

data Expr = IntLit  Int
          | Url     String String [String] (Maybe String) (Maybe String)
    deriving (Show)

parseError :: [Token] -> a
parseError _ = error "Parse error"
}
