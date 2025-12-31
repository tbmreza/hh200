{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Hh200.Scanner

-- import Language.LSP.Server (options, interpretHandler, staticHandlers, doInitialize, onConfigChange, parseConfig, configSection, defaultConfig, ServerDefinition(..), LspM, Handlers, defaultOptions, runLspT, Iso(..), (<~>)(Iso), runServer, notificationHandler)
import           Language.LSP.Server
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
  , TextDocumentIdentifier(..)
  , DidChangeConfigurationParams(..)
  )

import           Control.Monad.IO.Class
import qualified Data.Text.IO as T
import qualified Data.Text as Text
import           Data.Aeson hiding (defaultOptions)

data Config = Config { verbose :: Bool }
    deriving (Show)

instance FromJSON Config where
    parseJSON = withObject "Config" $ \v -> Config <$> v .:? "verbose" .!= False

validate :: Uri -> [Diagnostic]
validate _ = []

handlers :: Handlers (LspM Config)
handlers = mconcat
  [ notificationHandler SMethod_Initialized $ \_not -> do
      liftIO $ T.putStrLn "Server initialized"
  , notificationHandler SMethod_TextDocumentDidSave $ \msg -> do
      let TNotificationMessage _ _ (DidSaveTextDocumentParams (TextDocumentIdentifier uri) _) = msg
          diags = validate uri
      sendNotification SMethod_TextDocumentPublishDiagnostics $
        PublishDiagnosticsParams uri Nothing diags
  , notificationHandler SMethod_WorkspaceDidChangeConfiguration $ \_msg -> do
      -- (auto) This handler satisfies the LSP client that the notification is recognized.
      -- The actual configuration update is managed by the library via `onConfigChange`.
      liftIO $ T.putStrLn "Received workspace/didChangeConfiguration"
      return ()
  ]

-- (auto) This callback is invoked by the library (via `onConfigChange`) when the 
-- configuration is updated, typically following a `workspace/didChangeConfiguration` 
-- notification and successful parsing.
handleConfig :: Config -> LspM Config ()
handleConfig cfg = do
    liftIO $ T.putStrLn $ "Config changed: " <> Text.pack (show cfg)
    pure ()

sh :: a -> Handlers (LspM Config)
sh caps = handlers

configResult :: Config -> Value -> Either Text.Text Config
configResult old v =
    case fromJSON v of
        Success newConfig -> Right newConfig
        Error e -> Left (Text.pack e)

-- beforeResponse :: LanguageContextEnv config -> TMessage 'Method_Initialize -> IO (Either (TResponseError 'Method_Initialize) a)
beforeResponse env req = pure $ Right env

ih env = Iso (runLspT env) liftIO

main :: IO ()
main = do
    _int <- runServer $
        ServerDefinition
          { defaultConfig =    Config False
          , configSection =    "hh200d"
          , parseConfig =      configResult
          , onConfigChange =   handleConfig
          , doInitialize =     beforeResponse
          , staticHandlers =   sh
          , interpretHandler = ih
          , options =          defaultOptions {optServerInfo = Just (ServerInfo "??: this compiles but isn't effective at setting server doc at LspInfo" (Just "runtime red wall dismissable, server still attached"))}
          -- , options = defaultOptions
          }
    pure ()
