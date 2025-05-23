{
module L where
}
%wrapper "posn"

$white = [\ \t]
$newline = [\n\r]
$alpha = [a-zA-Z]
$digit = [0-9]
$path = [$alpha $digit \\ \/ \- \_ \.]

tokens :-
    $white+   ;
    "#".*     ;
    $newline  { tok (\p _ -> LN p) }

    [Gg][Ee][Tt]
  | [Pp][Oo][Ss][Tt]
  | [Pp][Uu][Tt]
  | [Dd][Ee][Ll][Ee][Tt][Ee]
  | [Pp][Aa][Tt][Cc][Hh]
  | [Oo][Pp][Tt][Ii][Oo][Nn][Ss]
  | [Hh][Ee][Aa][Dd]              { tok (\p s -> METHOD p s) }

    \(       { tok (\p _ -> PAREN_OPN p) }
    \)       { tok (\p _ -> PAREN_CLS p) }
    \[       { tok (\p _ -> LIST_OPN p) }
    \]       { tok (\p _ -> LIST_CLS p) }
    :        { tok (\p _ -> COLON p) }
    \"       { tok (\p _ -> QUOTE p) }


    then     { tok (\p _ -> KW_THEN p) }
    HTTP     { tok (\p _ -> KW_HTTP p) }
    Config   { tok (\p _ -> KW_CONFIG p) }
    $digit+  { tok (\p s -> DIGITS p s) }
    [\.\/]   { tok (\p _ -> SEP p) }

    [$alpha \_] [$alpha $digit \- \_]*  { tok (\p s -> IDENTIFIER p s) }

    $path+                    { tok (\p s -> PATH p s) }
    http [$printable # \#]+   { tok (\p s -> URL p s) }
    \" [$printable # \"]+ \"  { tok (\p s -> QUOTED p s) }


{

tok f p s = f p s

data Token =
    LN  AlexPosn  -- (token line terminator)

  | DIGITS      AlexPosn String
  | IDENTIFIER  AlexPosn String

  | SEP      AlexPosn         -- /
  | METHOD   AlexPosn String  -- GET
  | VERSION  AlexPosn String  -- HTTP/1.1
  | STATUS   AlexPosn Int     -- 500

  | PAREN_OPN  AlexPosn
  | PAREN_CLS  AlexPosn
  | LIST_OPN   AlexPosn
  | LIST_CLS   AlexPosn
  | COLON      AlexPosn
  | QUOTE      AlexPosn

  | KW_THEN    AlexPosn
  | KW_HTTP    AlexPosn
  | KW_CONFIG  AlexPosn


  | PATH    AlexPosn String
  | URL     AlexPosn String
  | QUOTED  AlexPosn String
  deriving (Eq, Show)

tokenPosn (DIGITS p _) = p
}
