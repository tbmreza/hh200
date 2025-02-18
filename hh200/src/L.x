{
module L where
}
%wrapper "posn"

$digit = 0-9

tokens :-
    $digit+                    { tok (\p s -> TokenInt p (read s)) }

{
tok f p s = f p s

data Token = Skip AlexPosn
           | TokenInt   AlexPosn Int
    deriving (Show)
}
