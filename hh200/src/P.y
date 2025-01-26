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

    httpUpper  { TokenX }
    method     { TokenStr $$ }
%%

Program : Statements                { $1 }

Statements : Statement              { [$1] }
           | Statements Statement   { $1 ++ [$2] }

Statement : var '=' Expr ';'        { Assign $1 $3 }
          | print Expr ';'          { Print $2 }
          | httpUpper Expr          { Response $2 }  -- HTTP 201
          | method Expr             { RequestLine $1 $2 }  -- POST http://httpbin.org

Expr : int                          { IntLit $1 }
     | var                          { VarRef $1 }
     | Expr op Expr                 { BinOp $2 $1 $3 }
     | '(' Expr ')'                 { $2 }


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
    deriving (Show)

parseError :: [Token] -> a
parseError _ = error "Parse error"
}
