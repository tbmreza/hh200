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
    $newline  { tok (\p _ -> LN p) }

    [Gg][Ee][Tt]
  | [Pp][Oo][Ss][Tt]
  | [Pp][Uu][Tt]
  | [Dd][Ee][Ll][Ee][Tt][Ee]
  | [Pp][Aa][Tt][Cc][Hh]
  | [Oo][Pp][Tt][Ii][Oo][Nn][Ss]
  | [Hh][Ee][Aa][Dd]              { tok (\p s -> METHOD p s) }

    \[       { tok (\p _ -> LIST_OPN p) }
    \]       { tok (\p _ -> LIST_CLS p) }
    :        { tok (\p _ -> COLON p) }
    HTTP     { tok (\p _ -> KW_HTTP p) }
    Config   { tok (\p _ -> KW_CONFIG p) }
    $digit+  { tok (\p s -> DIGITS p s) }
    [\.\/]   { tok (\p _ -> SEP p) }

    http [.]*  { tok (\p s -> RAW p s) }

    [$alpha \_] [$alpha $digit \- \_]*  { tok (\p s -> IDENTIFIER p s) }

    $path+     { tok (\p s -> PATH p s) }




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

  | LIST_OPN  AlexPosn
  | LIST_CLS  AlexPosn
  | COLON     AlexPosn

  | KW_HTTP    AlexPosn
  | KW_CONFIG  AlexPosn

  | URL_SCHEME     AlexPosn String -- https
  | URL_AUTHORITY  AlexPosn String -- example.com:8080
  | URL_PATH       AlexPosn String -- /path
  | URL_QUERY      AlexPosn String -- key=value&key2=value2
  | URL_FRAGMENT   AlexPosn String -- section1

  | PATH  AlexPosn String
  | RAW   AlexPosn String
  deriving (Eq, Show)

tokenPosn (DIGITS p _) = p
}
