{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hh200.Execution
  ( testShotgun
  , testOutsideWorld
  , testRps
  , runProcM
  , conduct
  , assertsAreOk
  , validJsonBody
  , ProcM
  , status200
  ) where

import Debug.Trace

import           Control.Concurrent.QSemN
import           Control.Exception        (bracket, bracket_, try, SomeException)
import           Control.Concurrent.Async (mapConcurrently, replicateConcurrently_)
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import           Control.Monad (foldM, forM, mzero, forever, void)
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
import           Hh200.TokenBucketWorkerPool (RateLimiter, RateLimiterConfig(..), initRateLimiter, waitAndConsumeToken)

-- | Execution context for a procedure.
data ExecContext = ExecContext
  { ecManager     :: Http.Manager
  -- , ecRateLimiter :: Maybe RateLimiter
  }

-- Procedure "may" fail early, "reads" an execution context (manager + optional rate limiter),
-- "writes" log as it runs, modifies environment "states" while doing IO.
type ProcM = MaybeT (Tf.RWST ExecContext Log Env IO)

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


-- | Low-level execution of a script. Returns the failing CallItem (if any), 
-- the final environment, and the execution log.
-- Nothing means the script finished successfully.
runProcM :: Script -> ExecContext -> Env -> IO (Maybe CallItem, Env, Log)
runProcM script ctx env = do
    let course :: ProcM CallItem = courseFrom script
    Tf.runRWST (runMaybeT course) ctx env

-- | High-level wrapper that orchestrates execution, catches exceptions, 
-- performs side effects (like tracing), and returns a Lead report.
-- conduct :: Script -> Http.Manager -> Env -> HostInfo -> IO Lead
-- conduct script mgr env hi = do
conduct :: Script -> ExecContext -> Env -> IO Lead
conduct script ctx env = do
    hi <- gatherHostInfo
    result <- try (runProcM script ctx env)
    case result of
        Left (e :: SomeException) -> do
            let errLog = [HttpError (show e)]
            -- We use defaultCallItem to indicate a system/execution error
            -- but we might want a more specific "Error" CallItem later.
            pure $ leadFrom (Just defaultCallItem) (env, errLog) script hi
        Right (mci, finalEnv, procLog) -> do
            traceM $ "Execution finished. Log length: " ++ show (length procLog)
            pure $ leadFrom mci (finalEnv, procLog) script hi

-- Env is modified, Log appended throughout the body of course construction.
--
-- A failing CallItem is not always found.
--
emptyHanded :: ProcM CallItem
emptyHanded = mzero

-- Exceptions:  when running ProcM
-- offline HttpExceptionRequest  -handling->  print
-- http client lib internal error  -handling->  halt (graceful if free)
courseFrom :: Script -> ProcM CallItem
courseFrom x = do
    lift $ Tf.tell [ScriptStart (length $ callItems x)]
    ctx <- ask
    go ctx (callItems x)

    where
    go :: ExecContext -> [CallItem] -> ProcM CallItem
    go _ [] = emptyHanded
    go ctx (ci:rest) = do
        lift $ Tf.tell [ItemStart (ciName ci)]
        
        -- -- Individual CallItem Rate Limiting  ??: is too fine-grain and not what we want
        -- case ecRateLimiter ctx of
        --     Nothing -> pure ()
        --     Just rl -> liftIO $ waitAndConsumeToken rl

        env <- get
        reqOrThrow <- liftIO $ buildRequest env ci

        -- Unhandled offline HttpExceptionRequest.
        -- ??: after exception handling sites are clear, print offline HttpExceptionRequest to user right away (or else).
        eitherResp <- liftIO ((try (Http.httpLbs reqOrThrow (ecManager ctx))) :: IO (Either Http.HttpException Http.Response))
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
                        go ctx rest

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

--------------------------------------------------------------------------------
-- hh200 modes
--------------------------------------------------------------------------------

-- | Gentle rate limit for sequential, single-VU execution.
-- Prevents accidental floods during development/debugging.
outsideWorldConfig :: RateLimiterConfig
outsideWorldConfig = RateLimiterConfig
  { bucketCapacity = 10   -- small burst headroom
  , refillRate     = 5    -- 5 rps sustained
  }

-- | Higher throughput for concurrent stress testing.
-- Capacity matches concurrency so all VUs can fire immediately on startup.
shotgunConfig :: Int -> RateLimiterConfig
shotgunConfig n = RateLimiterConfig
  { bucketCapacity = n     -- one token per VU
  , refillRate     = n * 2 -- sustain at 2x concurrency
  }

-- | Precise rate control for load testing with monitoring.
-- Already parameterized — this names the existing pattern from testRps.
rpsConfig :: Int -> RateLimiterConfig
rpsConfig rate = RateLimiterConfig
  { bucketCapacity = rate  -- matches rate for 1s burst tolerance
  , refillRate     = rate
  }

testOutsideWorld :: Script -> IO Lead
testOutsideWorld script = do
    bracket (Http.newManager (effectiveTls script))
            Http.closeManager $
            -- \mgr -> conduct script (ExecContext mgr Nothing) HM.empty
            \mgr -> conduct script (ExecContext mgr) HM.empty

-- plan:
-- hhs tests --> solid conduct in singular testOutsideWorld
-- tbwp --> general (i.e. testOutsideWorld and testShotgun) conduct sig
-- ??? --> load-testing-tool
testShotgun :: Int -> Script -> IO ()
testShotgun n script = do
    bracket (Http.newManager (effectiveTls script))
            Http.closeManager $
            \mgr -> do
                putStrLn $ "# testShotgun: n=" ++ show n
                -- _ <- mapConcurrentlyBounded n $ replicate n (runProcM script (ExecContext mgr Nothing) HM.empty)
                _ <- mapConcurrentlyBounded n $ replicate n (runProcM script (ExecContext mgr) HM.empty)
                pure ()

    where
    -- Limit concurrency with QSemN
    mapConcurrentlyBounded :: Int -> [IO a] -> IO [a]
    mapConcurrentlyBounded n actions = do
        sem <- newQSemN n
        mapConcurrently
            (\act -> bracket_ (waitQSemN sem 1)
                              (signalQSemN sem 1)
                              act)
            actions

-- Unminuted mode: a web service that listens to sigs for stopping hh200 from making calls.
-- RPS: rate of individual CallItems
testRps :: Int -> Int -> Script -> IO ()
testRps rpsVal concurrency script = do
    -- The web server is lazy: no start if no row is inserted to db.
    -- Inserts every second (or to a second-windowed timeseries data).
    connect "timeseries.db"

    bracket (Http.newManager (effectiveTls script))
            Http.closeManager $
            \mgr -> do
                rl <- initRateLimiter (RateLimiterConfig rpsVal rpsVal)
                -- let ctx = ExecContext mgr (Just rl)
                let ctx = ExecContext mgr
                putStrLn $ "# testRps: rate=" ++ show rpsVal ++ " reqs/sec, workers=" ++ show concurrency
                replicateConcurrently_ concurrency $ forever $ do
                    void $ runProcM script ctx HM.empty


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
