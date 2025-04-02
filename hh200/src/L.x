{
module L where
}
%wrapper "posn"

$alpha = [a-zA-Z]
$white = [\ \t]
$newline = [\n\r]

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

    HTTP    { tok (\p _ -> KW_HTTP p) }
    [0-9]+  { tok (\p s -> DIGITS p s) }
    [\.\/]  { tok (\p _ -> SYN p) }

    http [.]*  { tok (\p s -> RAW p s) }


{
tok f p s = f p s

data Token = SYN      AlexPosn
           | DIGITS   AlexPosn String
           | METHOD   AlexPosn String  -- GET
           | VERSION  AlexPosn String  -- HTTP/1.1
           | STATUS   AlexPosn Int     -- 500

           | KW_HTTP   AlexPosn

           | URL_SCHEME     AlexPosn String -- https
           | URL_AUTHORITY  AlexPosn String -- example.com:8080
           | URL_PATH       AlexPosn String -- /path
           | URL_QUERY      AlexPosn String -- key=value&key2=value2
           | URL_FRAGMENT   AlexPosn String -- section1

           | LN  AlexPosn  -- (token line terminator)

           | RAW   AlexPosn String

    deriving (Eq, Show)

tokenPosn (DIGITS p _) = p
}
