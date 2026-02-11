{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hh200.Execution
  ( testShotgun
  , testOutsideWorld
  , testRps
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
-- import qualified Data.Aeson.Types as Aeson (Value(..))

import qualified Network.HTTP.Client as HC
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
    { requestStruct = Nothing
    , method = "GET"
    , lexedUrl = "http://localhost:80"
    , headers = RhsDict HM.empty
    , configs = RhsDict HM.empty
    , payload = ""
    }
  , ciResponseSpec = Nothing
  }

mkLead :: Lead
mkLead = Lead
  { leadKind = Non
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

asMethod :: String -> BS.ByteString
asMethod s = BS.pack s

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


-- False indicates for corresponding CallItem (perhaps on user assert) to be
-- reported.
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
        results :: [BEL.Expr] <- mapM (BEL.eval env) assertionLines
        let values = map BEL.finalValue (trace ("results:" ++ show results) $ results)
            hasFailure = Aeson.Bool False `elem` (trace ("values:" ++ show values)$ values)

        if hasFailure then do
            putStrLn "# False assertion found"
            pure False
        else
            pure True

expectCodesOrDefault :: Maybe ResponseSpec -> [Status]
expectCodesOrDefault mrs =
    case mrs of
        Nothing -> [status200]
        Just rs -> case statuses rs of
            [] -> [status200]
            expectCodes -> expectCodes


-- ??: help me define type sig for runProcM that is general across all callsites: testOutsideWorld testRps and testShotgun
-- Return to user the CallItem which we suspect will fail again.
runProcM :: Script -> Http.Manager -> Env -> HostInfo -> IO Lead
runProcM script mgr env hi = do
    let course :: ProcM CallItem = courseFrom script
        list = runMaybeT course
    (mci, finalEnv, procLog) <- Tf.runRWST list mgr env
    pure $ switch (mci, finalEnv, procLog)

  -- -- ┌───────────┬──────────────────────────────────────┬─────────────────────────┐
  -- -- │ Location  │ Cause                                │ Result                  │
  -- -- ├───────────┼──────────────────────────────────────┼─────────────────────────┤
  -- -- │ buildFrom │ Http.parseRequest (Malformed URL)    │ Exception (IO)          │ checked: handled by alex/happy
  -- -- │ h         │ Http.httpLbs (Connection/Timeout)    │ Just ci (Captured Left) │ checked: some handled on the spot, some returned
  -- -- │ h         │ assertsAreOk (Logic/Status mismatch) │ Just ci (Logic Branch)  │ checked: exceptions if any will be falsified
  -- -- │ h         │ BEL.render / evalCaptures            │ Exception (IO)          │ checked: never throws
  -- -- └───────────┴──────────────────────────────────────┴─────────────────────────┘

    where
    switch :: (Maybe CallItem, Env, Log) -> Lead
    switch (mci, e, l) =
        trace ("final: " ++ show l) $ case mci of
            -- Just ci | "default" == ciName ci -> nonLead script hi
            -- _                                -> leadFromg mci (e, l) script hi
            -- ??: echoScript Nothing

            _ -> undefined
            -- Just ci | "default" == ciName ci -> mkLead { gfirstFailing = mci, ginterpreterInfo = (e, l), echoScript = Nothing, ghostInfo = hi }
            -- _                                -> mkLead { gfirstFailing = mci, ginterpreterInfo = (e, l), echoScript = Nothing, ghostInfo = hi }

-- Env is modified, Log appended throughout the body of course construction.
--
-- A failing CallItem is not always found, we encode None/Nothing
-- value with defaultCallItem to simplify code.
--
emptyHanded :: ProcM CallItem
emptyHanded = pure defaultCallItem

-- Exceptions:  when running ProcM
-- offline HttpExceptionRequest  -handling->  print
-- http client lib internal error  -handling->  halt (graceful if free)
courseFrom :: Script -> ProcM CallItem
courseFrom x = do
    lift $ Tf.tell [ScriptStart (length $ callItems x)]
    mgr <- ask
    go mgr (callItems x)

    where
    go :: Http.Manager -> [CallItem] -> ProcM CallItem
    go _ [] = emptyHanded
    go mgr (ci:rest) = do
        lift $ Tf.tell [ItemStart (ciName ci)]
        env <- get
        reqOrThrow <- liftIO $ buildRequest env ci

        -- Unhandled offline HttpExceptionRequest.
        -- ??: after exception handling sites are clear, print offline HttpExceptionRequest to user right away (or else).
        eitherResp <- liftIO ((try (Http.httpLbs reqOrThrow mgr)) :: IO (Either Http.HttpException Http.Response))
        case eitherResp of
            Left e -> do
                -- https://hackage-content.haskell.org/package/http-client-0.7.19/docs/src/Network.HTTP.Client.Types.html#HttpException
                -- ??
                lift $ Tf.tell [HttpError (show e)]
                pure ci
            Right gotResp -> do
                lift $ Tf.tell [HttpStatus (statusCode $ Http.getStatus gotResp)]
                -- Captures.
                -- env is already fetched above.

                let !initialEnv = HM.insert "RESP_BODY" (validJsonBody reqOrThrow gotResp) env

                let mrs = ciResponseSpec ci
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
                        go mgr rest

    -- Exceptions:
    -- request construction retry error
    buildRequest :: Env -> CallItem -> IO Http.Request
    buildRequest env CallItem { ciRequestSpec = RequestSpec { requestStruct = opt } } = do
        case opt of
            Just r -> pure (trace "buildRequest..." r)
            _ ->
                -- ??: env didn't exist during alex phase so maybe requestStruct will succeed here with env, allow env to contain directives/defaults
                undefined

    -- Reduce captures to Env extensions.
    evalCaptures :: (BEL.Env, Maybe ResponseSpec) -> IO (b0 -> BEL.Env, [TraceEvent])
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

-- Course is procedure in a stack form that will return the CallItem
-- that turned out to be failing.
-- courseFrom :: Script -> ProcM CallItem
-- courseFrom x = do
--     lift $ Tf.tell [ScriptStart (length $ callItems x)]
--     mgr <- ask
--     go mgr (callItems x)
--
--     where
--     go :: Http.Manager -> [CallItem] -> ProcM CallItem
--     go _ [] = emptyHanded
--     go mgr (ci:rest) = do
--         lift $ Tf.tell [ItemStart (ciName ci)]
--         env <- get
--         req <- liftIO $ buildRequestk env ci
--
--         -- Unhandled offline HttpExceptionRequest.
--         eitherResp <- liftIO ((try (Http.httpLbs req mgr)) :: IO (Either Http.HttpException Http.Response))
--         case eitherResp of
--             Left e -> do
--                 -- https://hackage-content.haskell.org/package/http-client-0.7.19/docs/src/Network.HTTP.Client.Types.html#HttpException
--                 -- ??
--                 lift $ Tf.tell [HttpError (show e)]
--                 pure ci
--             Right gotResp -> do
--                 lift $ Tf.tell [HttpStatus (statusCode $ Http.getStatus gotResp)]
--                 -- Captures.
--                 -- env is already fetched above.
--
--                 let !initialEnv = HM.insert "RESP_BODY" (validJsonBody req gotResp) env
--
--                 let mrs = ciResponseSpec ci
--                 (upsertCaptures, captureLog) <- liftIO (evalCaptures (initialEnv, mrs))
--
--                 lift $ Tf.tell captureLog
--
--                 -- Unless null Captures:
--                 modify upsertCaptures
--
--                 res <- liftIO $ assertsAreOk initialEnv gotResp mrs
--                 case res of
--                     False -> do
--                         lift $ Tf.tell [AssertsFailed]
--                         pure ci
--                     _ -> do
--                         lift $ Tf.tell [AssertsPassed]
--                         go mgr rest
--
--     -- Exceptions:
--     -- url parsing error
--     buildRequestk :: Env -> CallItem -> IO Http.Request
--     buildRequestk env ci
--         -- Requests without body.
--         | null (payload $ ciRequestSpec ci) = do
--             struct <- requestStructOrThrow
--
--             renderedHeaders <- renderHeaders (headers $ ciRequestSpec ci)
--
--             pure $
--                 Http.setRequestHeaders renderedHeaders $
--                 Http.setMethod (asMethod (method $ ciRequestSpec ci))
--                 struct
--
--         -- Requests with json body.
--         | otherwise = do
--             struct <- requestStructOrThrow
--             -- Render payloads.
--             rb <- stringRender (payload $ ciRequestSpec ci)
--
--             pure $
--                 Http.setRequestBody (trace ("rawPayload\t" ++ rb ++ ";") (rawPayload rb)) $
--                 Http.setRequestHeaders [headerJson] $
--                 Http.setMethod (asMethod (method $ ciRequestSpec ci))
--                 struct
--
--         where
--         renderHeaders :: RhsDict -> IO [(HeaderName, BS.ByteString)]
--         renderHeaders (RhsDict bindings) = do
--             traverseKV bindings $
--                 \(k :: String) (v :: [BEL.Part]) -> do
--                     av <- BEL.render env (Aeson.String "") v
--                     pure (CaseInsensitive.mk (BS.pack k), asBS av)
--
--         requestStructOrThrow :: IO Http.Request
--         requestStructOrThrow = do
--         -- ??
--             rendered <- stringRender (lexedUrl $ ciRequestSpec ci)
--             Http.parseRequest rendered
--
--         stringRender :: String -> IO String
--         stringRender s = do
--
--             rendered :: Aeson.Value <- BEL.render env (Aeson.String "") (BEL.partitions $ Text.pack s)
--
--             pure $ stringOrMt rendered
--
--         stringOrMt :: Aeson.Value -> String
--         stringOrMt v = Text.unpack $ textOrMt v
--
--         rawPayload :: String -> Http.RequestBody
--         rawPayload s = Http.lbsBody $ L8.pack s
--
--     -- Reduce captures to Env extensions.
--     evalCaptures :: (BEL.Env, Maybe ResponseSpec) -> IO (b0 -> BEL.Env, [TraceEvent])
--     evalCaptures (env, mrs) = do
--         let (RhsDict bindings) = case mrs of
--                 Nothing -> RhsDict HM.empty
--                 Just rs -> captures rs
--             initialLog = if HM.null bindings then [] else [CapturesStart (HM.size bindings)]
--         (ext, finalLog) <- foldlWithKeyM'
--             (\(acc, logs) bK (bV :: [BEL.Part]) -> do
--                 v <- BEL.render acc (Aeson.String "") bV
--                 pure (HM.insert bK v acc, logs ++ [Captured bK]))
--             (env, initialLog)
--             bindings
--         pure (const ext, finalLog)

--------------------------------------------------------------------------------
-- hh200 modes
--------------------------------------------------------------------------------

testOutsideWorld :: Script -> IO Lead

-- ??: sole `Script`s in testOutsideWorld (i.e. not testRps/testShotgun) probably 
-- don't need manager sharing. where does this bit fit in the stack?
testOutsideWorld sole@(Script {callItems = [_]}) = do
    hi <- trace "toww 1" gatherHostInfo

    bracket (Http.newManager True) Http.closeManager $ \with ->
        runProcM sole with HM.empty hi

    pure $ mkLead { leadKind = Non, echoScript = Just sole, hostInfo = hi }

testOutsideWorld flow@(Script {callItems = _}) = do
    undefined



-- Unminuted mode: a web service that listens to sigs for stopping hh200 from making calls.
testRps :: Script -> IO ()
testRps _ = do
    -- The web server is lazy: no start if no row is inserted to db.
    -- Inserts every second (or to a second-windowed timeseries data).
    connect "timeseries.db"
    pure ()

testShotgun :: Int -> Script -> IO ()
testShotgun n checked = do
    hi <- gatherHostInfo
    bracket (Http.newManager (effectiveTls checked)) Http.closeManager $ \with -> do
        let msg = "testShotgun: checked=" ++ show checked
        -- _ <- trace msg $ mapConcurrentlyBounded n $ replicate n $
        --     runProcM checked with HM.empty hi
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
