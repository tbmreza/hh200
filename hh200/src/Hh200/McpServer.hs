-- MCP (Model Context Protocol) control server over stdio.
--
-- Bridges JSON-RPC 2.0 tool calls to the same Unix domain socket that the CLI
-- run listener is bound to. It is a transport bridge, not a run controller:
-- it reuses Hh200.Dashboard.controlSocketPath / controlSocketWriter verbatim,
-- exactly as the dashboard's POST /api/sig does.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hh200.McpServer
  ( runMcpStdio
  , grammarResourceUri
  , grammarResourceText
  , grammarResourceContents
  , readResource
  ) where

import           Control.Exception (IOException, try)
import           Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import           Data.FileEmbed (embedStringFile, makeRelativeToProject)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import           Data.Text (Text)
import qualified Data.Text as T
import           System.IO (BufferMode (LineBuffering), hFlush, hGetLine, hSetBuffering, stdin, stdout)

import           Hh200.Dashboard (controlSocketPath, controlSocketWriter)

-- | Serve MCP (JSON-RPC 2.0) over newline-delimited stdio.
runMcpStdio :: IO ()
runMcpStdio = do
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout LineBuffering
    loop
  where
    loop = do
        lineE <- try (hGetLine stdin) :: IO (Either IOException String)
        case lineE of
            Left _ -> pure ()  -- stdin EOF: exit normally
            Right line -> do
                processLine line
                loop

processLine :: String -> IO ()
processLine line =
    case eitherDecode (BL.fromStrict (C8.pack line)) :: Either String Value of
        Left _ -> respond parseErrorResponse
        Right val -> handleMessage val

respond :: Value -> IO ()
respond v = do
    BL.hPut stdout (encode v <> "\n")
    hFlush stdout

parseErrorResponse :: Value
parseErrorResponse = object
    [ "jsonrpc" .= ("2.0" :: Text)
    , "id" .= Null
    , "error" .= object
        [ "code" .= (-32700 :: Int)
        , "message" .= ("Parse error" :: Text)
        ]
    ]

handleMessage :: Value -> IO ()
handleMessage (Object obj) =
    case methodName obj of
        Just "initialize" -> withId obj $ \i ->
            respond $ mkResponse i (initializeResult obj)
        -- Notification: never answered.
        Just "notifications/initialized" -> pure ()
        Just "tools/list" -> withId obj $ \i ->
            respond $ mkResponse i toolsListResult
        Just "tools/call" -> withId obj $ \i -> do
            r <- callTool obj
            respond $ mkResponse i r
        Just "resources/list" -> withId obj $ \i ->
            respond $ mkResponse i resourcesListResult
        Just "resources/read" -> withId obj $ \i ->
            respond $ mkResponse i (readResource obj)
        Just "ping" -> withId obj $ \i ->
            respond $ mkResponse i (object [])
        _ -> withId obj $ \i ->
            respond $ mkError i (-32601) "Method not found"
handleMessage _ = pure ()

withId :: Object -> (Value -> IO ()) -> IO ()
withId obj k =
    case KM.lookup "id" obj of
        Just i -> k i
        Nothing -> pure ()  -- notification: no response

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
mkError idVal code msg = object
    [ "jsonrpc" .= ("2.0" :: Text)
    , "id" .= idVal
    , "error" .= object
        [ "code" .= code
        , "message" .= msg
        ]
    ]

initializeResult :: Object -> Value
initializeResult obj =
    let params = paramsOf obj
        protocolVersion = case KM.lookup "protocolVersion" params of
            Just (String s) -> s
            _ -> "2025-06-18"
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
    T.pack $(makeRelativeToProject "src/Hh200/hhs-grammar.md" >>= embedStringFile)

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

readResource :: Object -> Value
readResource obj =
    case KM.lookup "uri" (paramsOf obj) of
        Just (String uri) | uri == grammarResourceUri -> grammarResourceContents
        _ -> object [ "contents" .= ([] :: [Value]) ]

callTool :: Object -> IO Value
callTool obj =
    case KM.lookup "name" (paramsOf obj) of
        Just (String s) -> dispatch (T.unpack s)
        _ -> pure $ toolError "" "tool name must be a string"
  where
    dispatch verb =
        case verb of
            "pause" -> exec verb
            "resume" -> exec verb
            "stop" -> exec verb
            _ -> pure $ toolError verb ("unknown tool: " ++ verb)

    exec verb = do
        path <- controlSocketPath
        r <- try (controlSocketWriter path (BL.fromStrict (C8.pack verb)))
                :: IO (Either IOException ())
        case r of
            Right () -> pure $ toolSuccess verb
            Left e -> pure $ toolError verb (show e)

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
