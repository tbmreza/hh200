{
module P where

import Debug.Trace

import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HM
import           Control.Monad.Trans.Except
import           L
import           Hh200.Types
import qualified BEL

}
%name parse
%tokentype { Token }
%error { parseError }
%token
    d           { DIGITS _ $$ }
    status      { STATUS _ $$ }
    identifier  { IDENTIFIER _ $$ }


    newline        { LN _ }

    "="         { SEP _ }
    "/"         { SEP _ }
    "."         { SEP _ }
    ","         { SEP _ }
    ":"         { COLON _ }
    "\""        { QUOTE _ }
    "{"         { BRACE_OPN _ }
    "}"         { BRACE_CLS _ }
    "("         { PAREN_OPN _ }
    ")"         { PAREN_CLS _ }
    "["         { LIST_OPN _ }
    "]"         { LIST_CLS _ }
    "then"      { KW_THEN _ }
    "HTTP"      { KW_HTTP _ }
    "Configs"   { KW_CONFIGS _ }
    "Captures"  { KW_CAPTURES _ }
    "Asserts"   { KW_ASSERTS _ }

    method      { METHOD _ $$ }
    header      { HEADER _ $$ }

    url         { URL _ $$ }
    s           { QUOTED _ $$ }
    braced      { BRACED _ $$ }
    rhs         { RHS _ $$ }

    jsonpath    { JSONPATH _ $$ }

    line         { LINE _ $$ }


%monad { E } { thenE } { returnE }

%%

script : call_items    { Script { config = defaultScriptConfig, callItems = $1 } }
       | response_asserts { Script { config = dbgScriptConfig, callItems = [] } }

crlf : {- optional newline -} { }
     | crlf newline           { }

deps_clause : deps "then" s { DepsClause { deps = $1, itemName = $3 } }
            | s             { DepsClause { deps = [], itemName = $1 } }

deps : s      { [$1] }
     | deps s { $1 ++ [$2] }


request  : method url crlf bindings braced crlf { RequestSpec { verb = expectUpper    $1, url = $2, headers = $4, payload = $5, opts = [] } }
         | method url crlf bindings crlf        { RequestSpec { verb = expectUpper    $1, url = $2, headers = $4, payload = "", opts = [] } }
         | method url crlf braced crlf                 { RequestSpec { verb = expectUpper    $1, url = $2,               payload = $4, opts = [] } }
         | method url crlf                             { RequestSpec { verb = expectUpper    $1, url = $2,               payload = "", opts = [] } }
         | url crlf                                    { RequestSpec { verb = expectUpper "GET", url = $1,               payload = "", opts = [] } }

response : "HTTP" response_codes crlf response_captures crlf response_asserts crlf
         { trace "rs1" $ ResponseSpec { asserts = $6, captures = $4, output = [], statuses = map statusFrom $2 } }

         | "HTTP" response_codes crlf response_captures crlf
         { trace "rs2" $ ResponseSpec { asserts = [], captures = $4, output = [], statuses = map statusFrom $2 } }

         | "HTTP" response_codes crlf
         { trace "rs3" $ ResponseSpec { asserts = [], captures = RhsDict HM.empty, output = [], statuses = map statusFrom $2 } }

         | response_captures crlf response_asserts crlf
         { trace "rs4" $ ResponseSpec { asserts = [], captures = $1, output = [], statuses = [] } }

         | response_captures crlf
         { trace "rs5" $ ResponseSpec { asserts = [], captures = $1, output = [], statuses = [] } }

response_captures :: { RhsDict }
response_captures : "[" "Captures" "]" crlf bindings { $5 }


bindings :: { RhsDict }
bindings : binding          { RhsDict (HM.fromList [$1]) }
         | bindings binding { let (RhsDict acc) = $1 in
                              RhsDict (HM.insert (fst $2) (snd $2) acc) }

binding :: { Binding }
binding : identifier ":" jsonpath crlf { ($1, BEL.L "$3") }
        | identifier ":" rhs crlf      { ($1, BEL.L "$3") }
        | identifier ":" s crlf        { ($1, BEL.R "$3") }


response_asserts :: { [String] }
response_asserts : "[" "Asserts" "]" crlf expr_lines { $5 }

expr_lines : line crlf           { [$1] }
           | expr_lines line { ($1 ++ [$2]) }

response_codes :: { [Int] }
response_codes : d                { [read $1] }
               | response_codes d { $1 ++ [read $2] }


call_item : deps_clause request response { trace "call_itemA" (pCallItem $1 $2 (Just $3)) }
          | deps_clause request          { trace "call_itemB" (pCallItem $1 $2 Nothing) }
          | request response             { trace ("call_itemC: " ++ show $2) pCallItem defaultDepsClause $1 (Just $2) }
          | request                      { trace "call_itemD" (pCallItem defaultDepsClause $1 Nothing) } 


call_items : call_item crlf           { [$1] }
           | call_items call_item crlf  { $1 ++ [$2] }

{

-- getLineNum :: Token -> Int
-- getLineNum (Token (AlexPn _ line _) _) = line
-- traceWithLine :: Token -> String -> a -> a
-- traceWithLine tok msg val = trace (msg ++ " at line " ++ show (getLineNum tok)) val

statusFrom :: Int -> Status
statusFrom n = mkStatus n ""


parseError :: [Token] -> E a
parseError tokens = failE $ "Parse error on tokens: " ++ show tokens

data E a = ParseOk a | ParseFailed String

thenE :: E a -> (a -> E b) -> E b
m `thenE` k =
   case m of
       ParseOk a     -> k a
       ParseFailed e -> ParseFailed e

returnE :: a -> E a
returnE a = ParseOk a

failE :: String -> E a
failE err = ParseFailed err

catchE :: E a -> (String -> E a) -> E a
catchE m k =
   case m of
      ParseOk a     -> ParseOk a
      ParseFailed e -> k e
}
