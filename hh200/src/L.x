{
module L where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
    "HTTP"              { \_ -> TokenX }
    [Gg][Ee][Tt]        { \s -> TokenStr s }
    [Pp][Oo][Ss][Tt]    { \s -> TokenStr s }

    $white+                    ;
    "--".*                     ;
    $digit+                    { \s -> TokenInt (read s) }
    "+" | "-" | "*" | "/"      { \s -> TokenOp s }
    "="                        { \s -> TokenAssign }
    "print"                    { \s -> TokenPrint }
    $alpha($digit|$alpha)*     { \s -> TokenVar s }
    \;                         { \s -> TokenSemicolon }
    \(                         { \s -> TokenLParen }
    \)                         { \s -> TokenRParen }

{
data Token 
    = TokenInt Int
    | TokenOp String
    | TokenAssign
    | TokenPrint
    | TokenVar String
    | TokenSemicolon
    | TokenLParen
    | TokenRParen

    | TokenX
    | TokenStr  String
    deriving (Show)
}
