{
module L where
}

-- <hurl-file> ::= <request>*
-- 
-- <request> ::= <request-line>
              -- <headers>?
              -- <body>?
              -- <response>*
-- 
-- <request-line> ::= <method> <space> <url>
-- 
-- <method> ::= "GET" | "POST" | "PUT" | "DELETE" | "HEAD" | "PATCH"
-- 
-- <url> ::= <scheme> "://" <host> <path>?
-- 
-- <headers> ::= <header> ("\n" <header>)*
-- 
-- <header> ::= <header-name> ":" <header-value>
-- 
-- <body> ::= <json-body> | <xml-body> | <raw-body>
-- 
-- <response> ::= "HTTP/" <version> <space> <status-code>
               -- <headers>?
               -- <body-check>?
-- 
-- <status-code> ::= <integer>
-- 
-- <body-check> ::= <json-check> | <xml-check> | <string-check>
-- 
-- <json-check> ::= "jsonpath" <string>
               -- | "jsonpath" <predicate>
-- 
-- <version> ::= "1.0" | "1.1" | "2.0"


%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$hex = [0-9a-fA-F]
$urlchar = [$alpha $digit \+ \- \. \_ \~ \! \$ \& \' \( \) \* \+ \, \; \= \:]


tokens :-
    "HTTP"                  { \_ -> Skip }
    [Gg][Ee][Tt]
  | [Pp][Oo][Ss][Tt]
  | [Pp][Aa][Tt][Cc][Hh]
  | [Pp][Uu][Tt]            { \s -> TMethod s }

    ---------
    -- URL --
    ---------
    $alpha [$alpha $digit \+ \- \.]* ":"   { \s -> URLScheme (init s) }

    -- Authority = userinfo + host + port
    "//" [^ \/\?\#]*                       { \s -> URLAuthority (drop 2 s) }

    "/" ($urlchar|"%"$hex$hex)*            { \s -> URLPath s }

    \? ($urlchar|"%"$hex$hex|\/|\?)*       { \s -> URLQuery (tail s) }

    \# ($urlchar|"%"$hex$hex|\/|\?)*       { \s -> URLFragment (tail s) }


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

    | Skip
    | TMethod  String
    | URLScheme String    -- https
    | URLAuthority String -- example.com:8080
    | URLPath String      -- /path
    | URLQuery String     -- key=value&key2=value2  ??
    | URLFragment String  -- section1  ??
    deriving (Show)
}
