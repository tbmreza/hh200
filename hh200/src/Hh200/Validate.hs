{-# LANGUAGE OverloadedStrings #-}

-- | Validation of an inline @.hhs@ script for the MCP @validate_script@ tool.
--
-- Runs only the lexer and the LR parser; there is no type checking yet, so a
-- script that parses cleanly is reported @ok@ even if its BEL expressions are
-- semantically bogus. A validation failure is a normal result carrying
-- structured diagnostics, not an error.
module Hh200.Validate
  ( DiagCode (..)
  , Diagnostic (..)
  , diagCodeText
  , validateScript
  ) where

import           Control.Monad.Trans.Except (runExceptT)
import           Data.Char (isDigit)
import           Data.List (isInfixOf, isPrefixOf)
import           Data.Text (Text)
import qualified Data.Text as T

import           Hh200.Scanner (AlexPosn (..), Token, getPos, parse, scanSafe)
import           Hh200.Types (Script)

-- | Kind of validation failure. The string forms match the MCP wire format.
data DiagCode = LexicalError | ParseError
  deriving (Eq, Show)

-- | A structured validation diagnostic with a 1-based position.
data Diagnostic = Diagnostic
  { diagLine    :: Int
  , diagCol     :: Int
  , diagCode    :: DiagCode
  , diagMessage :: String
  , diagHint    :: String
  } deriving (Eq, Show)

diagCodeText :: DiagCode -> String
diagCodeText LexicalError = "lexical_error"
diagCodeText ParseError    = "parse_error"

-- | Lex and parse an inline script, returning structured diagnostics for the
-- first failure. Pure apart from the vestigial @IO@ the parser still lives in.
validateScript :: Text -> IO (Either [Diagnostic] Script)
validateScript txt =
    case scanSafe (T.unpack txt) of
        Left err -> pure (Left [lexicalDiag err])
        Right tokens -> do
            res <- runExceptT (parse tokens)
            case res of
                Left (err, errTokens) -> pure (Left [parseDiag err errTokens])
                Right itemsAction -> do
                    res2 <- runExceptT itemsAction
                    case res2 of
                        Left (err, errTokens) -> pure (Left [parseDiag err errTokens])
                        Right s -> pure (Right s)

lexicalDiag :: String -> Diagnostic
lexicalDiag err =
    let (line, col) = extractLexPos err
    in Diagnostic line col LexicalError err (lexicalHint err)

-- | Parse @lexical error at line N, column M@; fall back to @(1,1)@ for
-- messages that carry no position (e.g. @lexical error: unbalanced braces@).
extractLexPos :: String -> (Int, Int)
extractLexPos msg =
    let prefix = "lexical error at line "
    in if prefix `isPrefixOf` msg
        then
            let rest = drop (length prefix) msg
                (lineStr, rest2) = span isDigit rest
                rest3 = dropWhile (not . isDigit) rest2
                (colStr, _) = span isDigit rest3
            in case (reads lineStr, reads colStr) of
                ([(l, _)], [(c, _)]) -> (l, c)
                _ -> (1, 1)
        else (1, 1)

lexicalHint :: String -> String
lexicalHint msg
    | "unbalanced braces" `isInfixOf` msg =
        "Ensure every '{' has a matching '}'."
    | "escaped EOF" `isInfixOf` msg =
        "Close the unterminated string with a double quote."
    | otherwise =
        "Unexpected character; check the token at this position."

parseDiag :: String -> [Token] -> Diagnostic
parseDiag err tokens =
    let (line, col) = case tokens of
            (t : _) -> tokenPos t
            [] -> (1, 1)
    in Diagnostic line col ParseError (firstLine err) parseHint

tokenPos :: Token -> (Int, Int)
tokenPos t = case getPos t of
    AlexPn _ line col -> (line, col)

-- | Drop Happy's trailing token dump ("Parse error on tokens:\n  ...").
firstLine :: String -> String
firstLine = takeWhile (/= '\n')

parseHint :: String
parseHint = "Check the syntax against the hh200 grammar."
