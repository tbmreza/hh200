{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | TokenBucketWorkerPool uses this module to execute Scripts.
module Hh200.Execution
  ( runScriptM

  , runProcM
  , conduct
  -- , validJsonBody
  , ProcM
  , status200
  -- , rhsDictToResponseHeaders
  , renderHeadersMap
  ) where

import Debug.Trace

import           System.IO (hPutStrLn, stderr, stdout)

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.QSemN
import           Control.Exception        (bracket, bracket_, try, SomeException)
import           Control.Concurrent.Async (mapConcurrently, replicateConcurrently_)
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import           Control.Monad (foldM, forM, mzero, forever, void)
import           Control.Monad (forM_, replicateM, replicateM_, when)
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

import           Control.Lens ((&), (?~))
import           Control.Lens.At (at)

import qualified Network.HTTP.Client as HC
import           Network.HTTP.Types.Status
import           Network.HTTP.Types.Header (HeaderName, ResponseHeaders)

import qualified BEL
import           BEL (responseCopy)
import qualified Hh200.Http as Http
import           Hh200.Types
import           Hh200.Graph (connect)
import           Hh200.Scanner (gatherHostInfo)
import           Hh200.ContentType (headerJson)

expectHeadersOrMt :: CallItem -> RhsDict
expectHeadersOrMt ci =
    case ciResponseSpec ci of
        Nothing -> RhsDict HM.empty
        Just rs -> rpResponseHeaders rs

-- -- | Convert a RhsDict (spec-side expected headers) to the canonical
-- -- ResponseHeaders type.
-- -- render :: Env -> Aeson.Value -> [Part] -> IO Aeson.Value
-- rhsDictToResponseHeaders :: RhsDict -> ResponseHeaders
-- rhsDictToResponseHeaders (RhsDict hm) =
--     [ ( CaseInsensitive.mk (TE.encodeUtf8 (Text.pack k))
--       , TE.encodeUtf8 (Text.concat (map partToText parts))
--       )
--     | (k, parts) <- HM.toList hm
--     ]
--
--     where
--     partToText :: BEL.Part -> Text
--     partToText (BEL.R t) = t
--     partToText (BEL.L t) = t  -- ??:

-- Procedure "may" fail early, "reads" an execution context (manager + optional rate limiter),
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
    , rqMethod = "GET"
    , lexedUrl = "http://localhost:80"
    , rqHeaders = RhsDict HM.empty
    , rqConfigs = RhsDict HM.empty
    , rqBody = ""
    }
  , ciResponseSpec = Nothing
  }

asMethod :: String -> BS.ByteString
asMethod s = BS.pack s

headersToAeson :: [(HeaderName, BS.ByteString)] -> Aeson.Value
headersToAeson hdrs = Aeson.Object $ KeyMap.fromList $ 
    map (\(k, v) -> (Key.fromText (Text.pack (BS.unpack (CaseInsensitive.original k))), Aeson.String (TE.decodeUtf8With TEE.lenientDecode v))) hdrs

asBS :: Aeson.Value -> BS.ByteString
asBS (Aeson.String t) = TE.encodeUtf8 t
asBS v                = BL.toStrict (Aeson.encode v)

textOrMt :: Aeson.Value -> Text
textOrMt (Aeson.String t) = t
textOrMt _ = ""


runScriptM :: Script -> Env -> IO ()
runScriptM script env = do
    let course :: ProcM CallItem = courseFrom script
    mgr <- Http.newManager True
    _ <- Tf.runRWST (runMaybeT course) mgr env
    pure ()

-- | Low-level execution of a script. Returns the failing CallItem (if any), 
-- the final environment, and the execution log.
-- Nothing means the script finished successfully.
runProcM :: Script -> Http.Manager -> Env -> IO (Maybe CallItem, Env, Log)
runProcM script mgr env = do
    let course :: ProcM CallItem = courseFrom script
    Tf.runRWST (runMaybeT course) mgr env

-- | High-level wrapper that orchestrates execution, catches exceptions, 
-- performs side effects (like tracing), and returns a Lead report.
conduct :: Script -> Http.Manager -> Env -> IO Lead
conduct script mgr env = do
    hi <- gatherHostInfo
    result <- try (runProcM script mgr env)
    case result of
        Left (e :: SomeException) -> do
            let errLog = [HttpError (show e)]
            -- We use defaultCallItem to indicate a system/execution error
            -- but we might want a more specific "Error" CallItem later.
            pure $ leadFrom (Just defaultCallItem) (env, errLog) script hi
        Right (mci, finalEnv, procLog) -> do
            traceM $ "Execution finished. Log length: " ++ show (length procLog)
            pure $ leadFrom mci (finalEnv, procLog) script hi

    where
    leadFrom :: Maybe CallItem -> (Env, Log) -> Script -> HostInfo -> Lead
    leadFrom failed el script hi = Lead
      { leadKind = Normal
      , firstFailing = failed
      , hostInfo = hi
      , echoScript = Just script
      , interpreterInfo = el
      }


ciCapturesOrMt :: CallItem -> RhsDict
ciCapturesOrMt ci =
    case ciResponseSpec ci of
        Nothing -> mtRhsDict
        Just rs -> rpCaptures rs

expectCodesOr200 :: CallItem -> [Status]
expectCodesOr200 ci =
    case ciResponseSpec ci of
        Nothing -> [status200]
        Just rs -> case rpStatuses rs of
            [] -> [status200]
            expectCodes -> expectCodes

assertionLinesOrMt :: CallItem -> [Text]
assertionLinesOrMt ci =
    case ciResponseSpec ci of
        Nothing -> []
        Just rs -> rpAsserts rs


-- Exceptions:  when running ProcM
-- offline HttpExceptionRequest  -handling->  print
-- http client lib internal error  -handling->  halt (graceful if free)
courseFrom :: Script -> ProcM CallItem
courseFrom x = do
    lift $ Tf.tell [ScriptStart (length $ callItems x)]
    mgr <- ask
    go mgr (callItems x)

    where
    buildRequest :: Env -> CallItem -> IO Http.Request
    buildRequest env CallItem { ciRequestSpec = RequestSpec { rqMethod, rqUrl } } = do
        case rqUrl of
            LexedUrlFull s -> do
                req <- HC.parseRequest s
                pure $ req { HC.method = BS.pack rqMethod }
            LexedUrlSegments parts -> do
                -- ??: if stronger commitment to returnE in parser is needed
                -- v <-        BEL.render acc (Aeson.String "") bV
                -- rendered <- BEL.render env' (Aeson.String "") parts
                undefined

    go :: Http.Manager -> [CallItem] -> ProcM CallItem
    go _ [] = mzero
    go mgr (ci:rest) = do
        lift $ Tf.tell [ItemStart (ciName ci)]

        env <- get
        reqOrThrow <- liftIO $ buildRequest env ci

        -- Unhandled offline HttpExceptionRequest.
        -- ??: after exception handling sites are clear, print offline HttpExceptionRequest to user right away (or else).
        eitherResp <- liftIO ((try (Http.httpLbs reqOrThrow mgr)) :: IO (Either Http.HttpException Http.Response))
        trace (present ci) $ case eitherResp of
            Left e -> do
                -- https://hackage-content.haskell.org/package/http-client-0.7.19/docs/src/Network.HTTP.Client.Types.html#HttpException

                lift $ Tf.tell [HttpError (show e)]
                pure ci
            Right gotResp -> do
                let envWithResp = env { BEL.responseCopy = gotResp
                                      , BEL.requestCopy = reqOrThrow
                                      }
                f <- liftIO (upsertCaptures envWithResp ci)
                modify f

                env' <- get

                ok <- liftIO (userAssertions env' ci)
                if not ok then
                    pure ci
                else
                    go mgr rest

    -- Status code assertion first, then all other checks (headers, body, and
    -- expressions about the response).
    userAssertions :: BEL.Env -> CallItem -> IO Bool
    userAssertions env' ci = do
        let expectList = expectCodesOr200 ci
            gotResp :: HC.Response L8.ByteString = responseCopy env'
            -- ??: alpha.hhs head on
            -- gotStatus = Http.getStatus gotResp
            -- gotStatus = status200
            gotStatus = status401

        if gotStatus `notElem` expectList then
            failWith ("status=" ++ show gotStatus ++ ", expect=" ++ show
                      expectList)
        else do
            -------------------------------------------------------------------
            -- Collect response headers checks. Can contain BEL parts.
            -------------------------------------------------------------------
            renderedHeaders <- renderHeadersMap env' (expectHeadersOrMt ci)

            let completeCheckedHeaders =
                    case length renderedHeaders of
                        -- Nothing to collect.
                        0 -> True
                        -- Default: assert subset of actual response headers.
                        _ ->
                            let gotRespHeaders =
                                    HM.fromList (HC.responseHeaders gotResp) in

                            HM.isSubmapOfBy (\ a b -> (asBS a) == b)
                                            renderedHeaders
                                            gotRespHeaders

            -------------------------------------------------------------------
            -- Check response body. Can contain BEL parts.
            --
            -- Default ??: assert subset of actual response body if it's json.
            -------------------------------------------------------------------
            let Aeson.Object actualJsonBodyMap = validJsonBody (BEL.requestCopy env') gotResp
            let actualBodyHM = HM.fromList $ map (\(k, v) -> (Text.unpack (Key.toText k), v)) (KeyMap.toList actualJsonBodyMap)
            -- We don't have an expected JSON body mapped to RhsDict yet, so we use an empty map.
            let completeCheckedJsonBody = HM.isSubmapOfBy (==) HM.empty actualBodyHM

            -------------------------------------------------------------------
            -- Collect [Asserts] expressions checks.
            --
            -- BEL evaluates all lines at once (for desired effect of visible
            -- BEL prints), but single False indicates for the whole [Asserts]
            -- block to be failing.
            -------------------------------------------------------------------
            expressions <- BEL.mapEval env' (assertionLinesOrMt ci)
            let aesonValues = map BEL.finalValue expressions

            pure $ and [ Aeson.Bool False `notElem` aesonValues
                       , completeCheckedHeaders
                       , completeCheckedJsonBody
                       ]

    -- Reduce captures to Env extensions.
    upsertCaptures :: BEL.Env -> CallItem -> IO (b0 -> BEL.Env)
    upsertCaptures env' ci = do
        let RhsDict c = ciCapturesOrMt ci

        ext <- foldlWithKeyM'
            (\ acc bK (bV :: [BEL.Part]) -> do
                v <- BEL.render acc (Aeson.String "") bV
                pure (acc { BEL.bindings = HM.insert bK v (BEL.bindings acc) }))
            env'
            c

        pure (const ext)

validJsonBody :: Http.Request -> Http.Response -> Aeson.Value
validJsonBody req resp = Aeson.Object $
    KeyMap.fromList [ (Key.fromText "body", bodyValue)
                    , (Key.fromText "headers", headersToAeson (Http.getHeaders resp))
                    , (Key.fromText "status", Aeson.Number (fromIntegral $ statusCode $ Http.getStatus resp))
                    , (Key.fromText "request", requestValue)
                    ]

    where
    bodyBytes = Http.getBody resp
    bodyValue = fromMaybe (Aeson.String (TE.decodeUtf8With TEE.lenientDecode (BL.toStrict bodyBytes))) (Aeson.decode bodyBytes)
    requestValue = Aeson.Object $ KeyMap.fromList
        [ (Key.fromText "method", Aeson.String (TE.decodeUtf8With TEE.lenientDecode (HC.method req)))
        , (Key.fromText "headers", headersToAeson (HC.requestHeaders req))
        ]

failWith :: String -> IO Bool
failWith msg = hPutStrLn stderr msg >> pure False

--------------------------------------------------------------------------------
-- hh200 modes
--------------------------------------------------------------------------------

triggerEmergencyShutdown :: TVar Bool -> IO ()
triggerEmergencyShutdown flag = do
    putStrLn "🚨 EMERGENCY SHUTDOWN TRIGGERED"
    atomically $ writeTVar flag True


-- testOutsideWorld :: Script -> IO Lead
-- testOutsideWorld script = do
--     -- bracket (Http.newManager (effectiveTls script))
--     bracket (Http.newManager True)
--             Http.closeManager $
--             \mgr -> conduct script mgr HM.empty

-- | Render expected headers.
renderHeadersMap :: BEL.Env -> RhsDict
                 -> IO (HM.HashMap (CaseInsensitive.CI BS.ByteString) Aeson.Value)
renderHeadersMap env' (RhsDict expectHeaders) =
    foldM (\ acc (k, parts) -> do
            let ciKey = CaseInsensitive.mk (TE.encodeUtf8 k)
            rendered <- BEL.render env' (Aeson.String "") parts
            pure $ HM.insert ciKey rendered acc
        ) HM.empty (HM.toList expectHeaders)

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
