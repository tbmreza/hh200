{
module L where
}
%wrapper "monadUserState"

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

    \{       { tok (\p _ -> BRACE_OPN p) }
    \}       { tok (\p _ -> BRACE_CLS p) }
    \(       { tok (\p _ -> PAREN_OPN p) }
    \)       { tok (\p _ -> PAREN_CLS p) }
    \[       { tok (\p _ -> LIST_OPN p) }
    \]       { tok (\p _ -> LIST_CLS p) }
    :        { tok (\p _ -> COLON p) }
    \"       { tok (\p _ -> QUOTE p) }


    then     { tok (\p _ -> KW_THEN p) }
    HTTP     { tok (\p _ -> KW_HTTP p) }

    Configs  { tok (\p _ -> KW_CONFIGS p) }
    Form     { tok (\p _ -> KW_FORM p) }
    Cookies  { tok (\p _ -> KW_COOKIES p) }

    Captures { tok (\p _ -> KW_CAPTURES p) }
    Asserts  { tok (\p _ -> KW_ASSERTS p) }

    $digit+  { tok (\p s -> DIGITS p s) }
    [\.\/=]  { tok (\p _ -> SEP p) }

    http [$printable # [$newline $white \#]]+   { tok (\p s -> URL p s) }

    [$alpha \_] [$alpha $digit \- \_]*  { tok (\p s -> IDENTIFIER p s) }

    \{ $printable+ \}         { tok (\p s -> BRACED p s) }
    \" [$printable # \"]+ \"  { tok (\p s -> QUOTED p s) }
    \$ $printable+            { tok (\p s -> JSONPATH p s) }
    : $printable+             { tok (\p s -> RHS p s) }

    > $printable+   { tok (\p s -> LINE p (drop 1 s)) }

{

tok :: (AlexPosn -> String -> Token) -> AlexInput -> Int -> Alex Token
tok f (p, _, _, s) len = return (f p (take len s))

data AlexUserState = AlexUserState {
    usCount :: Int
}
alexInitUserState = AlexUserState { usCount = 0 }

alexEOF :: Alex Token
alexEOF = do
    (p, _, _, _) <- alexGetInput
    return (EOF p)

alexScanTokens :: String -> [Token]
alexScanTokens str = case runAlex str gather of
    Left err -> error err
    Right toks -> toks
  where
    gather = do
        t <- alexMonadScan
        case t of 
            EOF _ -> return []
            _ -> (t:) <$> gather

data Token =
    LN  AlexPosn  -- (token line terminator)

  | DIGITS      AlexPosn String
  | IDENTIFIER  AlexPosn String

  | SEP      AlexPosn         -- /
  | METHOD   AlexPosn String  -- GET

  | VERSION  AlexPosn String  -- HTTP/1.1


  | BRACE_OPN  AlexPosn
  | BRACE_CLS  AlexPosn
  | PAREN_OPN  AlexPosn
  | PAREN_CLS  AlexPosn
  | LIST_OPN   AlexPosn
  | LIST_CLS   AlexPosn
  | COLON      AlexPosn
  | QUOTE      AlexPosn

  | KW_THEN      AlexPosn
  | KW_HTTP      AlexPosn

  | KW_CONFIGS   AlexPosn
  | KW_FORM      AlexPosn
  | KW_COOKIES   AlexPosn

  | KW_CAPTURES  AlexPosn
  | KW_ASSERTS   AlexPosn

  | URL     AlexPosn String  -- $printable excluding #, space, newline, tab and return chars
  | QUOTED  AlexPosn String
  | BRACED  AlexPosn String
  | RHS     AlexPosn String

  | JSONPATH    AlexPosn String

  | LINE  AlexPosn String
  | EOF   AlexPosn
  deriving (Eq, Show)

tokenPosn (DIGITS p _) = p
}
