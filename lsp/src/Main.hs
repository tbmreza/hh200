{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Control.Monad (void)
import Control.Monad.IO.Class
import qualified Data.Text as T
import Language.LSP.Server
import Language.LSP.Types
import Language.LSP.Diagnostics
import Control.Lens ((^.))

-- | The entry point for the LSP server.
main :: IO ()
main = void $ runServer $ ServerDefinition
    { onConfigurationChange = const $ pure (Right ())
    , doInitialize = \env _req -> pure $ Right env
    , staticHandlers = handlers
    , interpretHandler = \env -> Iso (runLspT env) liftIO
    , options = defaultOptions
        { textDocumentSync = Just syncOptions
        }
    }

syncOptions :: TextDocumentSyncOptions
syncOptions = TextDocumentSyncOptions
    { _openClose = Just True
    , _change = Just TextDocumentSyncKind_Full
    , _willSave = Just False
    , _willSaveWaitUntil = Just False
    , _save = Just $ InR $ SaveOptions $ Just True
    }

-- | Define the handlers for the LSP server.
handlers :: Handlers (LspM ())
handlers = mconcat
    [ notificationHandler STextDocumentDidOpen $ \msg -> do
        let doc = msg ^. params . textDocument
            uri = doc ^. uri
            version = doc ^. version
            content = doc ^. text
        validate uri (Just version) content
    , notificationHandler STextDocumentDidChange $ \msg -> do
        let doc = msg ^. params . textDocument
            uri = doc ^. uri
            version = doc ^. version

        let changes = msg ^. params . contentChanges
        case changes of
            (List (change:_)) -> do
                 -- With TextDocumentSyncKind_Full, 'text' is the whole content.
                 let content = change ^. text
                 validate uri (Just version) content
            _ -> pure ()
    ]

-- | "Compile" the code and publish diagnostics.
-- For bootstrap, we look for the word "todo" or "fixme".
validate :: Uri -> Maybe Int32 -> T.Text -> LspM () ()
validate uri version content = do
    let diags = simpleAnalysis content
    publishDiagnostics 100 (toNormalizedUri uri) version (partitionBySource diags)

-- | Simple analysis to find "todo" or "fixme".
simpleAnalysis :: T.Text -> [Diagnostic]
simpleAnalysis content = concatMap (checkLine content) (zip [0..] (T.lines content))

checkLine :: T.Text -> (Int, T.Text) -> [Diagnostic]
checkLine _ (lineNum, lineText) =
    [ makeDiagnostic lineNum (startCol) (endCol) "Todo found!"
    | (startCol, endCol) <- findOccurrences "todo" lineText
    ] ++
    [ makeDiagnostic lineNum (startCol) (endCol) "Fixme found!"
    | (startCol, endCol) <- findOccurrences "fixme" lineText
    ]

findOccurrences :: T.Text -> T.Text -> [(Int, Int)]
findOccurrences needle haystack = go 0 haystack
  where
    go offset t
        | T.null t = []
        | otherwise =
            let (pre, post) = T.breakOn needle t
            in if T.null post
               then []
               else
                   let start = offset + T.length pre
                       len = T.length needle
                       end = start + len
                   in (start, end) : go end (T.drop len post)

makeDiagnostic :: Int -> Int -> Int -> T.Text -> Diagnostic
makeDiagnostic line start end msg = Diagnostic
    { _range = Range (Position (fromIntegral line) (fromIntegral start)) (Position (fromIntegral line) (fromIntegral end))
    , _severity = Just DiagnosticSeverity_Information
    , _code = Nothing
    , _source = Just "hh200"
    , _message = msg
    , _tags = Nothing
    , _relatedInformation = Nothing
    , _codeDescription = Nothing
    , _data = Nothing
    }
