{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | TokenBucketWorkerPool uses this module to execute Scripts.
module Hh200.Execution
  ( runScriptM

  , runProcM
  , conduct
  -- , userAssertions
  , validJsonBody
  , ProcM
  , status200
  , ExecContext(..)
  ) where

import Debug.Trace

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
import           Network.HTTP.Types.Header (HeaderName)

import qualified BEL
import qualified Hh200.Http as Http
import           Hh200.Types
import           Hh200.Graph (connect)
import           Hh200.Scanner (gatherHostInfo)
import           Hh200.ContentType (headerJson)

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

headersToAeson :: [(HeaderName, BS.ByteString)] -> Aeson.Value
headersToAeson hdrs = Aeson.Object $ KeyMap.fromList $ 
    map (\(k, v) -> (Key.fromText (Text.pack (BS.unpack (CaseInsensitive.original k))), Aeson.String (TE.decodeUtf8With TEE.lenientDecode v))) hdrs

asBS :: Aeson.Value -> BS.ByteString
asBS (Aeson.String t) = TE.encodeUtf8 t
asBS v                = BL.toStrict (Aeson.encode v)

textOrMt :: Aeson.Value -> Text
textOrMt (Aeson.String t) = t
textOrMt _ = ""


expectCodesOrDefault :: Maybe ResponseSpec -> [Status]
expectCodesOrDefault mrs =
    case mrs of
        Nothing -> [status200]
        Just rs -> case statuses rs of
            [] -> [status200]
            expectCodes -> expectCodes

runScriptM :: Script -> Env -> IO ()
runScriptM script env = do
    let course :: ProcM CallItem = courseFrom script
    mgr <- Http.newManager True
    let ctx = ExecContext { ecManager = mgr }
    _ <- Tf.runRWST (runMaybeT course) ctx env
    pure ()

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

                lift $ Tf.tell [HttpError (show e)]
                pure ci
            Right gotResp -> do
                let mrs = ciResponseSpec ci
                let envWithResp = env
                        { BEL.responseCopy = gotResp
                        , BEL.requestCopy = reqOrThrow
                        }
                (f, tLogs :: [TraceEvent]) <- liftIO (upsertCaptures (envWithResp, mrs))
                lift $ Tf.tell tLogs
                modify f
                env' <- get

                res <- liftIO $ userAssertions env' gotResp (ciResponseSpec ci)

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
    -- ??: where Request struct construction *statistically* takes place
    -- buildRequest env CallItem { ciRequestSpec = RequestSpec { requestStruct = opt } } = do
    buildRequest env CallItem { ciRequestSpec = RequestSpec { requestStruct = opt, lexedUrl } } = do
        -- req :: HC.Request <- HC.parseRequest lexedUrl
        case opt of
            -- Just r -> pure (trace "buildRequest..." r)
            Just r -> pure r
            _ -> do
                req <- HC.parseRequest lexedUrl
                -- ??: env didn't exist during alex phase so maybe requestStruct will succeed here with env, allow env to contain directives/defaults
                -- so the typical Request construction is in this arm. while at it, review Request recall in BEL.
                pure req

    -- ??: review BEL apis. old: render mapEval
    -- new api:
    -- run :: Env -> Text -> IO Expr


    -- Reduce captures to Env extensions.
    upsertCaptures :: (BEL.Env, Maybe ResponseSpec) -> IO (b0 -> BEL.Env, [TraceEvent])
    upsertCaptures (envW, mrs) = do
        let (RhsDict bindings) = case mrs of
                Nothing -> RhsDict HM.empty
                Just rs -> captures rs
            initialLog = if HM.null bindings then [] else [CapturesStart (HM.size bindings)]

        (ext, finalLog) <- foldlWithKeyM'
            (\(acc, logs) bK (bV :: [BEL.Part]) -> do
                v <- BEL.render acc (Aeson.String "") bV
                pure (acc { BEL.bindings = HM.insert bK v (BEL.bindings acc) }, logs ++ [Captured bK]))
            (envW, initialLog)
            bindings

        pure (const ext, finalLog)

-- False indicates for corresponding CallItem (perhaps on user assert) to be
-- reported.
userAssertions :: Env -> Http.Response -> Maybe ResponseSpec -> IO Bool
userAssertions env got mrs = do
    pure False
    -- -- Assertion: status code is as expected.
    -- -- Assertion: none of the lines in [Asserts] evaluates to false.
    -- let status = Http.getStatus got
    --     expectList = expectCodesOrDefault mrs
    --
    -- if status `notElem` expectList
    --     then failWith $ "# Status Mismatch: Got " ++ show status ++ ", Expected " ++ show expectList
    --     else do
    --         okHeaders <- checkHeaders
    --         if okHeaders
    --             then checkAssertions
    --             else pure False
    --
    -- where
    -- failWith :: String -> IO Bool
    -- failWith msg = putStrLn msg >> pure False
    --
    -- -- Extract lines safely; if Nothing, default to empty list
    -- assertionLines :: [Text]
    -- assertionLines = maybe [] (map Text.pack . asserts) mrs
    --
    -- checkAssertions :: IO Bool
    -- checkAssertions = do
    --     results :: [BEL.Expr] <- BEL.mapEval env assertionLines
    --     let values = map BEL.finalValue results
    --
    --     forM_ (zip assertionLines values) $ \(line, val) ->
    --         putStrLn $ "Assert: " ++ Text.unpack line ++ " -> " ++ show val
    --
    --     let hasFailure = Aeson.Bool False `elem` values
    --
    --     if hasFailure
    --         then failWith "# False assertion found"
    --         else pure True
    --
    -- -- ??: confuses request headers with response headers spec
    -- checkHeaders :: IO Bool
    -- checkHeaders = case mrs of
    --     Nothing -> pure True
    --     Just rs -> do
    --         let (RhsDict expected) = responseHeaders rs
    --         if HM.null expected
    --             then pure True
    --             else do
    --                 let actual = Http.getHeaders got
    --                 foldlWithKeyM' 
    --                     (\acc k vParts -> if not acc then pure False else do
    --                         rendered <- BEL.render env (Aeson.String "") vParts
    --                         let expectedVal = textOrMt rendered
    --                             actualVal = findHeader k actual
    --                         case actualVal of
    --                             Nothing -> failWith $ "# Missing header: " ++ k
    --                             Just av | av == expectedVal -> pure True
    --                             Just av -> failWith $ "# Header Mismatch [" ++ k ++ "]: Got " ++ Text.unpack (fromMaybe "" actualVal) ++ ", Expected " ++ Text.unpack expectedVal
    --                     )
    --                     True
    --                     expected
    --
    -- findHeader :: String -> [(HeaderName, BS.ByteString)] -> Maybe Text
    -- findHeader name hdrs = 
    --     let target = CaseInsensitive.mk (BS.pack name)
    --     in case lookup target hdrs of
    --         Nothing -> Nothing
    --         Just bs -> Just (TE.decodeUtf8With TEE.lenientDecode bs)


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
--             \mgr -> conduct script (ExecContext mgr) HM.empty

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
