{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hh200.Execution
  ( testOutsideWorld , testShotgun, testRps
  , runProcM
  , assertsAreOk
  , validJsonBody
  , ProcM
  , status200
  ) where

import Debug.Trace

import           Control.Concurrent.QSemN
import           Control.Exception        (bracket, bracket_, try)
import           Control.Concurrent.Async (mapConcurrently)
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import           Control.Monad (foldM, forM)
import qualified Control.Monad.Trans.RWS.Strict as Tf

import           Data.Maybe (fromMaybe)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Char8 as BS
import qualified Data.CaseInsensitive as CaseInsensitive
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text as Text
import           Data.Text (Text)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as Aeson (Value(..))

import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Client.TLS as HCT
import           Network.HTTP.Types.Status
import           Network.HTTP.Types.Header (HeaderName)

import qualified BEL
import qualified Hh200.Http as Http
import           Hh200.Types
import           Hh200.Graph (connect)
import           Hh200.Scanner (gatherHostInfo)
import           Hh200.ContentType (headerJson)

-- Procedure "may" fail early, "reads" a shared http-client manager instance,
-- "writes" log as it runs, modifies environment "states" while doing IO.
type ProcM = MaybeT (Tf.RWST Http.Manager Log Env IO)

-- Mechanically, this is a corollary to http-client's defaultRequest.
--
-- "A default request value, a GET request of localhost/:80, with an empty
-- request body." - http-client hoogle
defaultCallItem :: CallItem
defaultCallItem = CallItem
  { ciDeps = []
  , ciName = "default"
  , ciRequestSpec = RequestSpec
    { verb = expectUpper "GET"
    , url = "http://localhost:80"
    , headers = RhsDict HM.empty
    , payload = "", opts = []
    }
  , ciResponseSpec = Nothing
  }

nonLead :: Script -> HostInfo -> Lead
nonLead x hi = Lead
  { leadKind = Non
  , firstFailing = Nothing
  , hostInfo = hi
  , interpreterInfo = (HM.empty, [])
  , echoScript = Just x
  }

leadFrom :: Maybe CallItem -> (Env, Log) -> Script -> HostInfo -> Lead
leadFrom failed el script hi = Lead
  { leadKind = Normal
  , firstFailing = failed
  , hostInfo = hi
  , echoScript = Just script
  , interpreterInfo = el
  }

_debugLead :: Lead
_debugLead = Lead
  { leadKind = Debug
  , firstFailing = Nothing
  , hostInfo = defaultHostInfo
  , echoScript = Nothing
  , interpreterInfo = (HM.empty, [])
  }

asMethod :: UppercaseString -> BS.ByteString
asMethod (UppercaseString s) = BS.pack s

validJsonBody :: Http.Request -> Http.Response -> Aeson.Value
validJsonBody req resp = Aeson.Object $
    KeyMap.fromList [ (Key.fromText "body", bodyValue)
                    , (Key.fromText "headers", headersValue)
                    , (Key.fromText "status", Aeson.Number (fromIntegral $ statusCode $ Http.getStatus resp))
                    , (Key.fromText "request", requestValue)
                    ]

    where
    bodyBytes = Http.getBody resp
    bodyValue = fromMaybe (Aeson.String (TE.decodeUtf8With TEE.lenientDecode (BL.toStrict bodyBytes))) (Aeson.decode bodyBytes)
    headersValue = Aeson.Object $ KeyMap.fromList $ map (\(k, v) -> (Key.fromText (Text.pack (BS.unpack (CaseInsensitive.original k))), Aeson.String (TE.decodeUtf8With TEE.lenientDecode v))) (Http.getHeaders resp)
    requestValue = Aeson.Object $ KeyMap.fromList
        [ (Key.fromText "method", Aeson.String (TE.decodeUtf8With TEE.lenientDecode (HC.method req)))
        , (Key.fromText "headers", requestHeadersValue)
        ]
    requestHeadersValue = Aeson.Object $ KeyMap.fromList $ map (\(k, v) -> (Key.fromText (Text.pack (BS.unpack (CaseInsensitive.original k))), Aeson.String (TE.decodeUtf8With TEE.lenientDecode v))) (HC.requestHeaders req)

asBS :: Aeson.Value -> BS.ByteString
asBS (Aeson.String t) = TE.encodeUtf8 t
asBS v                = BL.toStrict (Aeson.encode v)

textOrMt :: Aeson.Value -> Text
textOrMt (Aeson.String t) = t
textOrMt _ = ""


-- False indicates for corresponding CallItem (perhaps on user assert) to be reported.
assertsAreOk :: Env -> Http.Response -> Maybe ResponseSpec -> IO Bool
assertsAreOk env got mrs = do
    let status =     Http.getStatus got
        expectList = expectCodesOrDefault mrs

    if status `notElem` expectList then do
        putStrLn $ "# Status Mismatch: Got " ++ show status ++ ", Expected " ++ show expectList
        pure False
    else
        checkAssertions

    where
    -- Extract lines safely; if Nothing, default to empty list
    assertionLines :: [Text]
    assertionLines = maybe [] (map Text.pack . asserts) mrs

    -- Whether False can't be found in values.
    checkAssertions :: IO Bool
    checkAssertions = do
        results <- mapM (BEL.eval env) assertionLines
        let values = map (BEL.finalValue env) results
        pure (Aeson.Bool False `notElem` values)

expectCodesOrDefault :: Maybe ResponseSpec -> [Status]
expectCodesOrDefault mrs =
    case mrs of
        Nothing -> [status200]
        Just rs -> case statuses rs of
            [] -> [status200]
            expectCodes -> expectCodes

-- Return to user the CallItem which we suspect will fail again.
runProcM :: Script -> Http.Manager -> Env -> HostInfo -> IO Lead
runProcM script mgr env hi = do
    (mci, finalEnv, procLog) <- Tf.runRWST (runMaybeT $ courseFrom script) mgr env
    pure $ switch (mci, finalEnv, procLog)

    where
    switch :: (Maybe CallItem, Env, Log) -> Lead
    switch (mci, e, l) =
        trace ("final: " ++ show l) $ case mci of
            Just ci | "default" == ciName ci -> nonLead script hi
            _                                -> leadFrom mci (e, l) script hi

-- Env is modified, Log appended throughout the body of `courseFrom`.
--
-- A failing CallItem is not always found, we encode None/Nothing
-- value with defaultCallItem to simplify code.
--
emptyHanded :: ProcM CallItem
emptyHanded = pure defaultCallItem

-- Course is procedure in a stack form that will return the CallItem
-- that turned out to be failing.
courseFrom :: Script -> ProcM CallItem
courseFrom x = do
    lift $ Tf.tell [ScriptStart (length $ callItems x)]
    env <- get
    pairs <- liftIO (mapM (buildFrom env) (callItems x))
    liftIOWithMgr pairs

    where
    -- Build Request and echo CallItem parts.
    buildFrom :: Env -> CallItem -> IO (Http.Request, (CallItem, Maybe ResponseSpec))
    buildFrom env ci
        -- Requests without body.
        | null (payload $ ciRequestSpec ci) = do
            struct <- parseUrl

            renderedHeaders <- renderHeaders (headers $ ciRequestSpec ci)

            dorp (ci, (ciResponseSpec ci)) $
                Http.setRequestHeaders renderedHeaders $
                Http.setMethod (asMethod (verb $ ciRequestSpec ci))
                struct

        -- Requests with json body.
        | otherwise = do
            struct <- parseUrl
            -- Render payloads.
            rb <- stringRender (payload $ ciRequestSpec ci)

            dorp (ci, (ciResponseSpec ci)) $
                Http.setRequestBody (trace ("rawPayload\t" ++ rb ++ ";") (rawPayload rb)) $
                Http.setRequestHeaders [headerJson] $
                Http.setMethod (asMethod (verb $ ciRequestSpec ci))
                struct

        where
        renderHeaders :: RhsDict -> IO [(HeaderName, BS.ByteString)]
        renderHeaders (RhsDict bindings) = do
            traverseKV bindings $
                \(k :: String) (v :: [BEL.Part]) -> do
                    av <- BEL.render env (Aeson.String "") v
                    pure (CaseInsensitive.mk (BS.pack k), asBS av)

        parseUrl :: IO Http.Request
        parseUrl = do
            -- Render urls.
            rendered <- stringRender (url $ ciRequestSpec ci)
            Http.parseRequest rendered

        -- Return swapped product (`dorp` is prod reversed).
        dorp :: a -> b -> IO (b, a)
        dorp a b = pure (b, a)

        stringRender :: String -> IO String
        stringRender s = do

            rendered :: Aeson.Value <- BEL.render env (Aeson.String "") (BEL.partitions $ Text.pack s)

            pure $ stringOrMt rendered

        stringOrMt :: Aeson.Value -> String
        stringOrMt v = Text.unpack $ textOrMt v

        rawPayload :: String -> Http.RequestBody
        rawPayload s = Http.lbsBody $ L8.pack s

    -- Results arrive here!
    liftIOWithMgr :: [(Http.Request, (CallItem, Maybe ResponseSpec))] -> ProcM CallItem
    liftIOWithMgr pairs = do
        mgr :: Http.Manager <- ask
        h mgr pairs

        where

        -- Reduce captures to Env extensions.
        evalCaptures (env, mrs) = do
            let (RhsDict bindings) = case mrs of
                    Nothing -> RhsDict HM.empty
                    Just rs -> captures rs
                initialLog = if HM.null bindings then [] else [CapturesStart (HM.size bindings)]
            (ext, finalLog) <- foldlWithKeyM'
                (\(acc, logs) bK (bV :: [BEL.Part]) -> do
                    v <- BEL.render acc (Aeson.String "") bV
                    pure (HM.insert bK v acc, logs ++ [Captured bK]))
                (env, initialLog)
                bindings
            pure (const ext, finalLog)

        -- response = {captures, asserts}. request = {configs ("options" in hurl), cookies}
        h :: Http.Manager -> [(Http.Request, (CallItem, Maybe ResponseSpec))] -> ProcM CallItem
        h mgr list = case list of
            [] -> emptyHanded

            (req, (ci, mrs)) : rest -> do
                lift $ Tf.tell [ItemStart (ciName ci)]
                -- Unhandled offline HttpExceptionRequest.
                eitherResp <- liftIO ((try (Http.httpLbs req mgr)) :: IO (Either Http.HttpException Http.Response))
                case eitherResp of
                    Left e -> do
                        -- https://hackage-content.haskell.org/package/http-client-0.7.19/docs/src/Network.HTTP.Client.Types.html#HttpException
                        -- ??
                        lift $ Tf.tell [HttpError (show e)]
                        pure ci
                    Right gotResp -> do
                        lift $ Tf.tell [HttpStatus (statusCode $ Http.getStatus gotResp)]
                        -- Captures.
                        env <- get

                        let !initialEnv = HM.insert "RESP_BODY" (validJsonBody req gotResp) env

                        (upsertCaptures, captureLog) <- liftIO (evalCaptures (initialEnv, mrs))

                        lift $ Tf.tell captureLog

                        -- Unless null Captures:
                        modify upsertCaptures

                        res <- liftIO $ assertsAreOk initialEnv gotResp mrs
                        case res of
                            False -> do
                                lift $ Tf.tell [AssertsFailed]
                                pure ci
                            _ -> do
                                lift $ Tf.tell [AssertsPassed]
                                h mgr rest

--------------------------------------------------------------------------------
-- hh200 modes
--------------------------------------------------------------------------------

testOutsideWorld :: Script -> IO Lead

-- -> NonLead
testOutsideWorld static@(Script {config = _, callItems = []}) = do
    hi <- gatherHostInfo
    pure $ nonLead static hi

-- -> NonLead | DebugLead | Lead
testOutsideWorld sole@(Script {config = ScriptConfig {subjects = _}, callItems = [_]}) = do
    hi <- gatherHostInfo
    bracket (Http.newManager (effectiveTls sole)) Http.closeManager $ \with ->
        runProcM sole with HM.empty hi


-- -> NonLead | DebugLead | Lead
testOutsideWorld flow@(Script {callItems = _}) = do
    hi <- gatherHostInfo
    bracket (Http.newManager (effectiveTls flow)) Http.closeManager $ \with ->
        runProcM flow with HM.empty hi




-- Unminuted mode: a web service that listens to sigs for stopping hh200 from making calls.
testRps :: Script -> IO ()
testRps _ = do
    -- The web server is lazy: no start if no row is inserted to db.
    -- Inserts every second (or to a second-windowed timeseries data).
    connect "timeseries.db"
    pure ()

-- Limit concurrency with QSemN
mapConcurrentlyBounded :: Int -> [IO a] -> IO [a]
mapConcurrentlyBounded n actions = do
    sem <- newQSemN n
    mapConcurrently
        (\act -> bracket_ (waitQSemN sem 1)
                          (signalQSemN sem 1)
                          act)
        actions

testShotgun :: Int -> Script -> IO ()
testShotgun n checked = do
    hi <- gatherHostInfo
    bracket (Http.newManager (effectiveTls checked)) Http.closeManager $ \with -> do
        let msg = "testShotgun: checked=" ++ show checked
        _ <- trace msg $ mapConcurrentlyBounded n $ replicate n $
            runProcM checked with HM.empty hi
        pure ()


-- thread's:  system start-end times  script success pct  memory
-- data DataPoint = DataPoint
--   { shotgunN :: Int
--   , shotgunPct :: Double  -- Percentage of user assertions satisfying responses.
--   }

-- mkDataPoint :: Int -> DataPoint
-- mkDataPoint n = DataPoint { shotgunN = n, shotgunPct = 0.0 }

--------------------------------------------------------------------------------
-- More lib than app code
--------------------------------------------------------------------------------
foldlWithKeyM' :: (Monad m) => (a -> k -> v -> m a)
                            -> a
                            -> HM.HashMap k v
                            -> m a
foldlWithKeyM' f z0 hm = foldM step z0 (HM.toList hm)
    where
    step !acc (k, v) = f acc k v

traverseKV :: HM.HashMap k v -> (k -> v -> IO a) -> IO [a]
traverseKV hm f =
    forM (HM.toList hm) $ \(k, v) -> f k v
