{
module P where
--    int        { TokenInt $$ }
-- Expr       : int                    { IntLit $1 }

import L
}
%name parse
%tokentype { Token }
%error { parseError }
%token 
    httpUpper  { Skip $$ }
%%

Program    : Statements             { $1 }

Statements : Statement              { [$1] }
           | Statements Statement   { $1 ++ [$2] }

Statement  : httpUpper Expr         { Response $2 }

Expr       : httpUpper                    { IntLit 1 }

{
data Statement 
    = Response  Expr
data Expr 
    = IntLit  Int

parseError :: [Token] -> a
parseError _ = error "Parse error"
}
