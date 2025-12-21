{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Main where

-- import Language.LSP.Server (options, interpretHandler, staticHandlers, doInitialize, onConfigChange, parseConfig, configSection, defaultConfig, ServerDefinition(..), LspM, Handlers, defaultOptions, runLspT, Iso(..), (<~>)(Iso), runServer, notificationHandler)
import Language.LSP.Server
import Language.LSP.Protocol.Types
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
  )

import Language.LSP.Protocol.Message
import Control.Monad.IO.Class
import qualified Data.Text.IO as T

validate :: Uri -> [Diagnostic]
validate _ = []

handlers :: Handlers (LspM ())
handlers = mconcat
  [ notificationHandler SMethod_Initialized $ \_not -> do
      liftIO $ T.putStrLn "Server initialized"
  , notificationHandler SMethod_TextDocumentDidSave $ \msg -> do
      let TNotificationMessage _ _ (DidSaveTextDocumentParams (TextDocumentIdentifier uri) _) = msg
          diags = validate uri
      sendNotification SMethod_TextDocumentPublishDiagnostics $
        PublishDiagnosticsParams uri Nothing diags
  ]

main :: IO ()
main = do
  _ <- runServer $ ServerDefinition
    { defaultConfig = ()
    , configSection = "hh200d"
    , parseConfig = const $ const $ Right ()
    , onConfigChange = const $ pure ()
    , doInitialize = \env _req -> pure $ Right env
    , staticHandlers = \_caps -> handlers
    , interpretHandler = \env -> Iso (runLspT env) liftIO
    -- , options = defaultOptions {optServerInfo = Just (ServerInfo "??: this compiles but isn't effective at setting server doc at LspInfo" (Just "runtime red wall dismissable, server still attached"))}
    , options = defaultOptions
    }
  pure ()
