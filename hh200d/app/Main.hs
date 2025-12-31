{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import qualified Hh200.Scanner

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
  )

import           Control.Monad.IO.Class
import qualified Data.Text.IO as T
import qualified Data.Text as Text
import           Data.Aeson hiding (defaultOptions)

-- TCP
import           Network.Socket
import           System.Environment (getArgs)
import           System.IO
import           Control.Monad (void, forever)
import           Control.Exception (bracket)
import           Colog.Core (LogAction (..), WithSeverity (..))

data Config = Config { verbose :: Bool }
    deriving (Show)

instance FromJSON Config where
    parseJSON = withObject "Config" $ \v -> Config <$> v .:? "verbose" .!= False

validate :: Uri -> LspM Config [Diagnostic]
validate uri = do
    mdoc <- getVirtualFile (toNormalizedUri uri)
    case mdoc of
        Nothing -> return []
        Just vf -> do
            let content = Text.unpack (virtualFileText vf)
            let errs = Hh200.Scanner.diagnostics content
            return $ map toDiagnostic errs

toDiagnostic :: ((Int, Int), String) -> Diagnostic
toDiagnostic ((l, c), msg) =
    let pos = Position (fromIntegral (l - 1)) (fromIntegral (c - 1))
        range = Range pos pos
    in Diagnostic range (Just DiagnosticSeverity_Error) Nothing Nothing (Just "hh200") (Text.pack msg) Nothing Nothing Nothing

validateAndPublish :: Uri -> LspM Config ()
validateAndPublish uri = do
    diags <- validate uri
    sendNotification SMethod_TextDocumentPublishDiagnostics $
        PublishDiagnosticsParams uri Nothing diags

handlers :: Handlers (LspM Config)
handlers = mconcat
  [ notificationHandler SMethod_Initialized $ \_not -> do
        liftIO $ T.putStrLn "Server initialized"

  , notificationHandler SMethod_TextDocumentDidSave $ \msg -> do
        let TNotificationMessage _ _ (DidSaveTextDocumentParams (TextDocumentIdentifier uri) _) = msg
        validateAndPublish uri

  , notificationHandler SMethod_TextDocumentDidOpen $ \msg -> do
        let TNotificationMessage _ _ (DidOpenTextDocumentParams (TextDocumentItem uri _ _ _)) = msg
        validateAndPublish uri

  , notificationHandler SMethod_TextDocumentDidChange $ \msg -> do
        let TNotificationMessage _ _ (DidChangeTextDocumentParams (VersionedTextDocumentIdentifier uri _) _) = msg
        validateAndPublish uri

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

beforeResponse :: LanguageContextEnv Config -> TMessage Method_Initialize -> IO (Either (TResponseError Method_Initialize) (LanguageContextEnv Config))
beforeResponse env _req = pure $ Right env

ih :: LanguageContextEnv Config -> LspM Config <~> IO
ih env = Iso (runLspT env) liftIO

ioLogger :: LogAction IO (WithSeverity LspServerLog)
ioLogger = LogAction $ \_ -> return ()

lspLogger :: LogAction (LspM Config) (WithSeverity LspServerLog)
lspLogger = LogAction $ \_ -> return ()

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--port", p] -> runTcp (read p)
        _             -> runStdio

lspServerDef :: ServerDefinition Config
lspServerDef =
    ServerDefinition
      { defaultConfig =    Config False
      , configSection =    "hh200d"
      , parseConfig =      configResult
      , onConfigChange =   handleConfig
      , doInitialize =     beforeResponse
      , staticHandlers =   sh
      , interpretHandler = ih
      , options =          defaultOptions {optServerInfo = Just (ServerInfo "??: this compiles but isn't effective at setting server doc at LspInfo" (Just "abc45commithash"))}
      }
-- PICKUP poke around: compile time commit hash, LspInfo activate multiple clients then kill by id

runStdio :: IO ()
runStdio = void $ runServer lspServerDef

runTcp :: Int -> IO ()
runTcp port = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet (fromIntegral port) (tupleToHostAddress (127, 0, 0, 1)))
    -- Expect one client (a text editor) to connect at a time.
    listen sock 2
    putStrLn $ "Listening on port " ++ show port
    forever $ do
        (conn, _addr) <- accept sock
        putStrLn "Connection accepted"
        hdl <- socketToHandle conn ReadWriteMode
        hSetBuffering hdl NoBuffering
        _ <- runServerWithHandles ioLogger lspLogger hdl hdl lspServerDef
        hClose hdl
