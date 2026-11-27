-- MCP (Model Context Protocol) control server over stdio.
--
-- Bridges JSON-RPC 2.0 tool calls to the same Unix domain socket that the CLI
-- run listener is bound to. It is a transport bridge, not a run controller:
-- it reuses Hh200.Control.controlSocketPath / controlSocketWriter verbatim,
-- exactly as the dashboard's POST /api/sig does.
--
-- Input is read as bytes and decoded as UTF-8 JSON whatever the locale, and
-- stdout carries nothing but JSON-RPC messages.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hh200.McpServer
  ( runMcpStdio
  , runMcp
  , handleRequest
  , supportedProtocolVersions
  , grammarResourceUri
  , grammarResourceText
  , grammarResourceContents
  , readResource
  , validateScriptResult
  ) where

import           Control.Exception (IOException, try)
import           Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import           Data.FileEmbed (embedFile, makeRelativeToProject)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import           System.IO (Handle, hFlush, hIsEOF, hSetBinaryMode, stdin, stdout)
import           System.IO.Error (isDoesNotExistError)

import           Hh200.Control (controlSocketPath, controlSocketWriter)
import           Hh200.Types (SrcSpan (..))
import qualified Hh200.Validate as Validate

-- | Serve MCP (JSON-RPC 2.0) over newline-delimited stdio.
runMcpStdio :: IO ()
runMcpStdio = runMcp stdin stdout

-- | Serve MCP on a pair of handles: one JSON-RPC message per line in, one per
-- line out. Returns when the input reaches end of file.
runMcp :: Handle -> Handle -> IO ()
runMcp hIn hOut = do
    hSetBinaryMode hIn True
    hSetBinaryMode hOut True
    loop
  where
    loop = do
        eof <- hIsEOF hIn
        if eof
            then pure ()
            else do
                line <- C8.hGetLine hIn
                let msg = stripCR line
                if C8.all (`elem` (" \t" :: String)) msg
                    then pure ()
                    else handleLine msg >>= mapM_ (respond hOut)
                loop

    stripCR bs
        | not (BS.null bs) && C8.last bs == '\r' = BS.init bs
        | otherwise = bs

-- | Parse one line and answer it; 'Nothing' for a notification.
handleLine :: BS.ByteString -> IO (Maybe Value)
handleLine line =
    case eitherDecodeStrict line :: Either String Value of
        Left _ -> pure (Just parseErrorResponse)
        Right val -> handleRequest val

respond :: Handle -> Value -> IO ()
respond hOut v = do
    BL.hPut hOut (encode v <> "\n")
    hFlush hOut

parseErrorResponse :: Value
parseErrorResponse = mkError Null (-32700) "Parse error"

-- | Answer one decoded JSON-RPC message. Notifications (no @id@) get
-- 'Nothing', whatever their method.
handleRequest :: Value -> IO (Maybe Value)
handleRequest (Object obj) =
    case KM.lookup "id" obj of
        Nothing -> pure Nothing
        Just i -> Just <$> answer i
  where
    answer i = case methodName obj of
        Just "initialize" -> pure $ mkResponse i (initializeResult obj)
        Just "tools/list" -> pure $ mkResponse i toolsListResult
        Just "tools/call" -> either (errorFrom i) (mkResponse i) <$> callTool obj
        Just "resources/list" -> pure $ mkResponse i resourcesListResult
        Just "resources/read" -> pure $ either (errorFrom i) (mkResponse i) (readResource obj)
        Just "ping" -> pure $ mkResponse i (object [])
        Just _ -> pure $ mkError i (-32601) "Method not found"
        Nothing -> pure $ mkError i (-32600) "Invalid Request"
    errorFrom i (RpcError code msg dat) = mkErrorWith i code msg dat
handleRequest _ = pure (Just (mkError Null (-32600) "Invalid Request"))

-- | A JSON-RPC error: code, message, optional data.
data RpcError = RpcError Int Text (Maybe Value)

methodName :: Object -> Maybe String
methodName obj =
    case KM.lookup "method" obj of
        Just (String s) -> Just (T.unpack s)
        _ -> Nothing

mkResponse :: Value -> Value -> Value
mkResponse idVal result = object
    [ "jsonrpc" .= ("2.0" :: Text)
    , "id" .= idVal
    , "result" .= result
    ]

mkError :: Value -> Int -> Text -> Value
mkError idVal code msg = mkErrorWith idVal code msg Nothing

mkErrorWith :: Value -> Int -> Text -> Maybe Value -> Value
mkErrorWith idVal code msg dat = object
    [ "jsonrpc" .= ("2.0" :: Text)
    , "id" .= idVal
    , "error" .= object (
        [ "code" .= code
        , "message" .= msg
        ] ++ maybe [] (\d -> ["data" .= d]) dat)
    ]

-- | Protocol revisions this server speaks, newest first.
supportedProtocolVersions :: [Text]
supportedProtocolVersions = [latestProtocolVersion, "2025-03-26", "2024-11-05"]

latestProtocolVersion :: Text
latestProtocolVersion = "2025-06-18"

-- | Agree to the client's revision when we speak it; otherwise offer our
-- newest and let the client decide whether to continue.
initializeResult :: Object -> Value
initializeResult obj =
    let params = paramsOf obj
        protocolVersion = case KM.lookup "protocolVersion" params of
            Just (String v) | v `elem` supportedProtocolVersions -> v
            _ -> latestProtocolVersion
    in object
        [ "protocolVersion" .= protocolVersion
        , "capabilities" .= object
            [ "tools" .= object []
            , "resources" .= object []
            ]
        , "serverInfo" .= object
            [ "name" .= ("hh200-mcp" :: Text)
            , "version" .= ("0.0.1" :: Text)
            ]
        ]

toolsListResult :: Value
toolsListResult = object
    [ "tools" .=
        [ tool "pause" "Pause the running hh200 load test."
        , tool "resume" "Resume a paused hh200 load test."
        , tool "stop" "Stop the running hh200 load test."
        , validateScriptTool
        ]
    ]

tool :: Text -> Text -> Value
tool name description = object
    [ "name" .= name
    , "description" .= description
    , "inputSchema" .= object
        [ "type" .= ("object" :: Text)
        , "properties" .= object []
        , "required" .= ([] :: [Text])
        ]
    ]

-- | The @validate_script@ tool advertises an inline @script@ string argument.
validateScriptTool :: Value
validateScriptTool = object
    [ "name" .= ("validate_script" :: Text)
    , "description" .= ("Validate an inline .hhs script without running it; parse errors are returned as a normal result." :: Text)
    , "inputSchema" .= object
        [ "type" .= ("object" :: Text)
        , "properties" .= object
            [ "script" .= object
                [ "type" .= ("string" :: Text)
                , "description" .= ("The draft .hhs script contents." :: Text)
                ]
            ]
        , "required" .= (["script"] :: [Text])
        ]
    ]

paramsOf :: Object -> Object
paramsOf obj =
    case KM.lookup "params" obj of
        Just (Object p) -> p
        _ -> KM.empty

--------------------------------------------------------------------------------
-- Static resources
--------------------------------------------------------------------------------

-- | URI of the single static grammar resource served over MCP.
grammarResourceUri :: Text
grammarResourceUri = "hh200://docs/hhs-grammar"

-- | Static grammar reference embedded in the binary at compile time. Served
-- via @resources\/read@ as @text\/markdown@ with no control-channel or
-- database access.
grammarResourceText :: Text
grammarResourceText =
    decodeUtf8 $(makeRelativeToProject "src/Hh200/hhs-grammar.md" >>= embedFile)

-- | The @resources\/read@ result payload for the grammar resource.
grammarResourceContents :: Value
grammarResourceContents = object
    [ "contents" .=
        [ object
            [ "uri" .= grammarResourceUri
            , "mimeType" .= ("text/markdown" :: Text)
            , "text" .= grammarResourceText
            ]
        ]
    ]

resourcesListResult :: Value
resourcesListResult = object
    [ "resources" .=
        [ object
            [ "uri" .= grammarResourceUri
            , "name" .= ("hhs-grammar" :: Text)
            , "description" .= ("hh200 DSL grammar reference" :: Text)
            , "mimeType" .= ("text/markdown" :: Text)
            ]
        ]
    ]

-- | Route a @resources\/read@ request. An unknown URI is the MCP
-- "Resource not found" error (@-32002@), with the URI as @data@.
readResource :: Object -> Either RpcError Value
readResource obj =
    case KM.lookup "uri" (paramsOf obj) of
        Just (String uri)
            | uri == grammarResourceUri -> Right grammarResourceContents
            | otherwise -> Left (RpcError (-32002) "Resource not found" (Just (object ["uri" .= uri])))
        _ -> Left (RpcError (-32602) "resources/read needs a string `uri`" Nothing)

-- | Route a @tools\/call@ request. An unknown tool is a protocol error
-- (@-32602@); a failure while running a known tool is a result with
-- @isError@ set, so the model can read it.
callTool :: Object -> IO (Either RpcError Value)
callTool obj =
    case KM.lookup "name" (paramsOf obj) of
        Just (String s) -> dispatch (T.unpack s)
        _ -> pure $ Left (RpcError (-32602) "tools/call needs a string `name`" Nothing)
  where
    dispatch verb =
        case verb of
            "pause" -> Right <$> exec verb
            "resume" -> Right <$> exec verb
            "stop" -> Right <$> exec verb
            "validate_script" -> Right <$> validateTool obj
            _ -> pure $ Left (RpcError (-32602) (T.pack ("Unknown tool: " ++ verb)) Nothing)

    exec verb = do
        path <- controlSocketPath
        r <- try (controlSocketWriter path (BL.fromStrict (C8.pack verb)))
                :: IO (Either IOException ())
        case r of
            Right () -> pure $ toolSuccess verb
            Left e
                | noListener e -> pure $ toolError verb
                    ("no hh200 run is listening on " ++ path
                     ++ " (start one with `hh200 --duration N SCRIPT` or from the dashboard)")
                | otherwise -> pure $ toolError verb (show e)

    -- ENOENT (no socket file) or ECONNREFUSED (a file but no listener).
    noListener e = isDoesNotExistError e
        || "refused" `T.isInfixOf` T.toLower (T.pack (show e))

toolSuccess :: String -> Value
toolSuccess verb = object
    [ "content" .=
        [ object
            [ "type" .= ("text" :: Text)
            , "text" .= (T.pack verb <> ": ok")
            ]
        ]
    , "isError" .= False
    ]

toolError :: String -> String -> Value
toolError verb msg = object
    [ "content" .=
        [ object
            [ "type" .= ("text" :: Text)
            , "text" .= (T.pack verb <> ": " <> T.pack msg)
            ]
        ]
    , "isError" .= True
    ]

--------------------------------------------------------------------------------
-- validate_script
--------------------------------------------------------------------------------

-- | Handle a @validate_script@ tool call. A validation failure is a normal
-- result (@isError@ stays @False@): the payload distinguishes @ok@/errors.
validateTool :: Object -> IO Value
validateTool obj =
    case KM.lookup "script" (toolArguments obj) of
        Just (String script) -> do
            payload <- validateScriptResult script
            pure $ object
                [ "content" .=
                    [ object
                        [ "type" .= ("text" :: Text)
                        , "text" .= decodeUtf8 (BL.toStrict (encode payload))
                        ]
                    ]
                , "isError" .= False
                ]
        _ -> pure $ toolError "validate_script" "the `script` argument must be a string"

-- | Validate an inline script and shape the result as
-- @{"ok":true}@ or @{"ok":false,"errors":[{"line","col","code","message","hint"}]}@.
validateScriptResult :: Text -> IO Value
validateScriptResult script = do
    r <- Validate.validateScript script
    pure $ case r of
        Right _ -> object [ "ok" .= True ]
        Left diags -> object
            [ "ok" .= False
            , "errors" .= map diagToJson diags
            ]

diagToJson :: Validate.Diagnostic -> Value
diagToJson d = object
    [ "line" .= fst (spanStart (Validate.diagSpan d))
    , "col" .= snd (spanStart (Validate.diagSpan d))
    , "code" .= Validate.diagCodeText (Validate.diagCode d)
    , "message" .= Validate.diagMessage d
    , "hint" .= Validate.diagHint d
    ]

-- | Extract the @arguments@ object from a @tools\/call@ request, if present.
toolArguments :: Object -> Object
toolArguments obj =
    case KM.lookup "arguments" (paramsOf obj) of
        Just (Object a) -> a
        _ -> KM.empty
