{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

module Hh200.LanguageServer (runTcp) where

-- PICKUP merge g; mv hh200d to trash
-- import Language.LSP.Server (options, interpretHandler, staticHandlers, doInitialize, onConfigChange, parseConfig, configSection, defaultConfig, ServerDefinition(..), LspM, Handlers, defaultOptions, runLspT, Iso(..), (<~>)(Iso), runServer, notificationHandler)
import           Language.LSP.Server
import           Language.LSP.VFS (virtualFileText, VirtualFile)
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types
  ( InitializeResult(..)
  , ServerCapabilities
  , ServerInfo(..)
  , Uri(..)
  , PublishDiagnosticsParams(..)
  , Diagnostic(..)
  , Range(..)
  , Position(..)
  , DiagnosticSeverity(..)
  , DidSaveTextDocumentParams(..)
  , DidOpenTextDocumentParams(..)
  , DidChangeTextDocumentParams(..)
  , TextDocumentItem(..)
  , VersionedTextDocumentIdentifier(..)
  , TextDocumentIdentifier(..)
  , DidChangeConfigurationParams(..)
  , toNormalizedUri
  , TextEdit(..)
  , DocumentRangeFormattingParams(..)
  , type (|?)(..)
  )

import           Control.Monad.IO.Class
import qualified Data.Text.IO as T
import qualified Data.Text as Text
import           Data.Aeson hiding (defaultOptions)
import           Development.GitRev (gitHash)


runTcp :: Int -> IO ()
runTcp port = do
    putStrLn $ "Run hh200 language server on port " ++ show port
    putStrLn "Not implemented yet"
