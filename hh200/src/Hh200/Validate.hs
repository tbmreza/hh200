-- | Validation of an inline @.hhs@ script for the MCP @validate_script@ tool.
--
-- Thin wrapper over 'Hh200.Scanner.parseWithDiagnostics'. Runs only the lexer
-- and the LR parser; there is no type checking yet, so a script that parses
-- cleanly is reported @ok@ even if its BEL expressions are semantically bogus.
-- A validation failure is a normal result carrying structured diagnostics, not
-- an error.
module Hh200.Validate
  ( DiagCode (..)
  , Diagnostic (..)
  , diagCodeText
  , validateScript
  ) where

import           Data.Text (Text)
import qualified Data.Text as T

import           Hh200.Scanner (parseWithDiagnostics)
import           Hh200.Types (DiagCode (..), Diagnostic (..), diagCodeText, Script)

-- | Lex and parse an inline script, returning structured diagnostics for the
-- first failure. Pure apart from the vestigial @IO@ the parser still lives in.
validateScript :: Text -> IO (Either [Diagnostic] Script)
validateScript = parseWithDiagnostics . T.unpack
