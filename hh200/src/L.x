{
module L where
}
%wrapper "posn"

$digit = 0-9

tokens :-
    $digit+                    { tok (\p s -> DIGITS p (read s)) }
    [Gg][Ee][Tt]
  | [Pp][Oo][Ss][Tt]
  | [Pp][Aa][Tt][Cc][Hh]
  | [Pp][Uu][Tt]               { tok (\p s -> METHOD p s) }


{
tok f p s = f p s

data Token = SKIP    AlexPosn
           | DIGITS  AlexPosn Int
           | METHOD  AlexPosn String

           | URL_SCHEME     AlexPosn String -- https
           | URL_AUTHORITY  AlexPosn String -- example.com:8080
           | URL_PATH       AlexPosn String -- /path
           | URL_QUERY      AlexPosn String -- key=value&key2=value2
           | URL_FRAGMENT   AlexPosn String -- section1

    deriving (Show)
}
