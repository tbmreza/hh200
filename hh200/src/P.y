{
module P where

import L
}

%name parse
%tokentype { Token }
%error { parseError }

%token 
    int     { TokenInt $$ }
    op      { TokenOp $$ }
    '='     { TokenAssign }
    print   { TokenPrint }
    var     { TokenVar $$ }
    ';'     { TokenSemicolon }
    '('     { TokenLParen }
    ')'     { TokenRParen }

    scheme     { URLScheme $$ }
    authority  { URLAuthority $$ }
    path       { URLPath $$ }
    query      { URLQuery $$ }
    fragment   { URLAuthority $$ }

    httpUpper  { Skip }
    method     { TMethod $$ }
%%

Program : Statements                { $1 }

Statements : Statement              { [$1] }
           | Statements Statement   { $1 ++ [$2] }

Statement : var '=' Expr ';'        { Assign $1 $3 }
          | print Expr ';'          { Print $2 }
          | httpUpper Expr          { Response $2 }        -- HTTP 201
          | method Expr             { RequestLine $1 $2 }  -- POST http://httpbin.org

Expr : int                          { IntLit $1 }

     -- | scheme authority paths queries fragments  { Url $1 $2 $3 $4 $5 }

     | scheme authority paths { Url $1 $2 $3 Nothing Nothing }

     -- | var                          { VarRef $1 }
     | Expr op Expr                 { BinOp $2 $1 $3 }
     | '(' Expr ')'                 { $2 }


paths : path paths      { $1 : $2 }
      | {- empty -}     { [] }

queries : query         { Just $1 }
        | {- empty -}   { Nothing }

fragments : fragment    { Just $1 }
          | {- empty -} { Nothing }

{
data Statement 
    = Assign String Expr
    | Print Expr

    | Response     Expr
    | RequestLine  String Expr
    deriving (Show)

data Expr 
    = IntLit  Int
    | VarRef  String
    | BinOp   String Expr Expr

    | Url  String String [String] (Maybe String) (Maybe String)
    deriving (Show)

parseError :: [Token] -> a
parseError _ = error "Parse error"
}
