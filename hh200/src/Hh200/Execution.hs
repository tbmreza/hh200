{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | TokenBucketWorkerPool uses this module to execute Scripts.
module Hh200.Execution
  ( runScriptM

  , runProcM
  , conduct
  , ProcM
  , status200
  , renderHeadersMap
  , objectSubset
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
-- import           Data.ByteString.Lazy (ByteString, fromStrict)
import           Data.ByteString.Lazy (fromStrict)
import qualified Data.Text as Text
import           Data.Text (Text)
import qualified Data.Aeson as Aeson (encode, decode, Value (..))
import           Data.Aeson (object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Vector as V

import           Control.Lens ((&), (?~))
import           Control.Lens.At (at)

import qualified Network.HTTP.Client as HC ( method
                                           , requestHeaders
                                           , responseHeaders
                                           , responseBody
                                           , Response
                                           , parseRequest
                                           , requestBody
                                           , RequestBody (RequestBodyBS, RequestBodyLBS))
import           Network.HTTP.Types.Status
import           Network.HTTP.Types.Header (HeaderName, ResponseHeaders)

import qualified BEL
import           BEL (responseCopy)
import qualified Hh200.Http as Http
import           Hh200.Types
import           Hh200.Graph (connect)
import           Hh200.Scanner (gatherHostInfo)
import           Hh200.ContentType (headerJson)

data SubsetResult =
    ASubsetOfB        -- A ⊆ B
  | BSubsetOfA        -- B ⊆ A
  | Equal             -- A = B (mutual subset)
  | Incomparable      -- neither is a subset of the other
  | InvalidJson Side  -- one or both inputs failed to parse
    deriving (Show, Eq)

data Side = SideA | SideB | BothSides deriving (Show, Eq)

-- | Entry point: takes strict ByteStrings
jsonSubset :: BS.ByteString -> BS.ByteString -> SubsetResult
jsonSubset a b =
  case (Aeson.decode (fromStrict a), Aeson.decode (fromStrict b)) of
    (Nothing, Nothing) -> InvalidJson BothSides
    (Nothing, _      ) -> InvalidJson SideA
    (_      , Nothing) -> InvalidJson SideB
    (Just va, Just vb) ->
      case (isSubset va vb, isSubset vb va) of
           (True,  True) ->  Equal
           (True,  False) -> ASubsetOfB
           (False, True) ->  BSubsetOfA
           (False, False) -> Incomparable

-- | @isSubset x y@ — is @x@ structurally contained within @y@?
--
-- Rules:
--   Null     ⊆  anything
--   Scalar   ⊆  same scalar
--   Array a  ⊆  Array b  iff every element of a has a match in b (multiset-style)
--   Object a ⊆  Object b iff every key in a exists in b with a subset value
--   mixed types → False
isSubset :: Aeson.Value -> Aeson.Value -> Bool
isSubset Aeson.Null _ = True
isSubset _ Aeson.Null = False  -- nothing is subset of Null

-- Scalar subset relation is eq check.
isSubset (Aeson.Bool x)   (Aeson.Bool y)   = x == y
isSubset (Aeson.Number x) (Aeson.Number y) = x == y
isSubset (Aeson.String x) (Aeson.String y) = x == y

isSubset (Aeson.Array xs)   (Aeson.Array ys)   = arraySubset (V.toList xs) (V.toList ys)
isSubset (Aeson.Object as_) (Aeson.Object bs_) = objectSubset (KeyMap.toHashMap as_) (KeyMap.toHashMap bs_)

isSubset _ _ = False  -- type mismatch

-- For example [1,1] ⊄ [1,2] but [1,2] ⊆ [1,1,2].
arraySubset :: [Aeson.Value] -> [Aeson.Value] -> Bool
arraySubset [] _  =    True
arraySubset (_:_) [] = False

arraySubset (x:xs) ys =
    case removeFirst (isSubset x) ys of
        Nothing  -> False
        Just ys' -> arraySubset xs ys'

removeFirst :: (a -> Bool) -> [a] -> Maybe [a]
removeFirst _ [] = Nothing
removeFirst p (x:xs)
  | p x       = Just xs
  | otherwise = (x :) <$> removeFirst p xs

type Object = HM.HashMap KeyMap.Key Aeson.Value

objectSubset :: Object -> Object -> Bool
objectSubset as_ bs_ =
    all (\(k, v) -> maybe False (isSubset v) (HM.lookup k bs_)) (HM.toList as_)


expectHeadersOrMt :: CallItem -> RhsDict
expectHeadersOrMt ci =
    case ciResponseSpec ci of
        Nothing -> RhsDict HM.empty
        Just rs -> rpResponseHeaders rs

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
    { rqMethod = "GET"
    -- , rqUrl = "http://localhost:80"
    , rqHeaders = RhsDict HM.empty
    -- , rqConfigs = RhsDict HM.empty
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
        Just rp ->
            case rpSquares rp of
                (Just (ResponseSquareCaptures d), _) -> d
                _ -> mtRhsDict

expectCodesOr200 :: CallItem -> [Status]
expectCodesOr200 ci =
    case ciResponseSpec ci of
        Nothing -> [status200]
        Just rs -> case rpStatuses rs of
            [] -> [status200]
            expectCodes -> expectCodes

assertionLinesOrMt :: CallItem -> [Text]
assertionLinesOrMt ci =
    -- case ciResponseSpec ci of
    case ciResponseSpec (trace ("assertionLinesOrMt:" ++ show ci) ci) of
        Nothing -> []
        Just rp ->
            case rpSquares rp of
                (_, Just (ResponseSquareAsserts d)) -> map Text.pack d
                _ -> []


-- Exceptions:  when running ProcM
-- offline HttpExceptionRequest  -handling->  print
-- http client lib internal error  -handling->  halt (graceful if free)
courseFrom :: Script -> ProcM CallItem
courseFrom x = do
    lift $ Tf.tell [ScriptStart (length $ callItems x)]
    mgr <- ask
    go mgr (callItems x)

                -- ??: interpolate in braced / json; stack install
    where
    buildRequest :: Env -> CallItem -> IO Http.Request
    buildRequest env CallItem { ciRequestSpec = RequestSpec { rqMethod, rqUrl, rqHeaders, rqBody } } = do
        case rqUrl of
            LexedUrlFull s -> do
                req <- HC.parseRequest s
                let encoded = BL.fromStrict (BS.pack rqBody)
                renderedReqHeaders <- renderRequestHeaders env rqHeaders
                pure $ req { HC.method = BS.pack rqMethod
                           , HC.requestHeaders = renderedReqHeaders
                           -- The type of requestBody as sent over the wire is
                           -- abstracted by the library. For example, to get
                           -- the desired effect of sending an object as JSON,
                           -- idiomatic Content-Type header is assumed.
                           , HC.requestBody = HC.RequestBodyLBS (trace ("encoded=" ++ show encoded) encoded)
                           }
            LexedUrlSegments parts -> do
                full <- BEL.render env (Aeson.String "") undefined
                undefined

    go :: Http.Manager -> [CallItem] -> ProcM CallItem
    go _ [] = mzero
    go mgr (ci:rest) = do
        lift $ Tf.tell [ItemStart (ciName ci)]

        env <- get
        reqOrThrow <- liftIO $ buildRequest env ci

        -- Unhandled offline HttpExceptionRequest.
        -- ??: after exception handling sites are clear, print offline HttpExceptionRequest to user right away (or else).
        -- eitherResp <- liftIO ((try (Http.httpLbs reqOrThrow mgr)) :: IO (Either Http.HttpException Http.Response))
        eitherResp <- let reqInfo = case HC.requestBody reqOrThrow of
                                    -- HC.RequestBodyLBS lbs -> "LBS " ++ show (BL.length lbs)
                                    HC.RequestBodyLBS lbs -> "LBS " ++ show lbs
                                    HC.RequestBodyBS bs -> "BS " ++ show (BS.length bs)
                        in trace ("built=" ++ reqInfo) $
                           liftIO ((try (Http.httpLbs reqOrThrow mgr)) :: IO (Either Http.HttpException Http.Response))
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
            gotStatus = Http.getStatus gotResp

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
            -- PICKUP subset checking
            -- haskell fn takes 2 ByteString args. if both parse as valid json strings, decide if one is subset of the other
            -- Default ??: assert subset of actual response body if it's json.
            -------------------------------------------------------------------
            let b :: L8.ByteString = HC.responseBody gotResp
            -- ??: render rp braced

            -- let Aeson.Object actualJsonBodyMap = validJsonBody (BEL.requestCopy env') gotResp
            -- let actualBodyHM = HM.fromList $ map (\(k, v) -> (Text.unpack (Key.toText k), v)) (KeyMap.toList actualJsonBodyMap)
            -- -- We don't have an expected JSON body mapped to RhsDict yet, so we use an empty map.
            -- let completeCheckedJsonBody = HM.isSubmapOfBy (==) HM.empty actualBodyHM

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
                       -- , completeCheckedJsonBody
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

-- | (auto) Render expected headers.
renderRequestHeaders :: BEL.Env -> RhsDict -> IO [(HeaderName, BS.ByteString)]
renderRequestHeaders env' (RhsDict reqHeaders) =
    foldM (\ acc (k, parts) -> do
            let ciKey = CaseInsensitive.mk (TE.encodeUtf8 k)
            rendered <- BEL.render env' (Aeson.String "") parts
            let bsValue = case rendered of
                    Aeson.String t -> TE.encodeUtf8 t
                    v -> BL.toStrict (Aeson.encode v)
            pure $ (ciKey, bsValue) : acc
        ) [] (HM.toList reqHeaders)

-- | Render expected response headers.
renderHeadersMap :: BEL.Env -> RhsDict
                 -> IO (HM.HashMap (CaseInsensitive.CI BS.ByteString) Aeson.Value)
renderHeadersMap env' (RhsDict expectHeaders) =
    foldM (\ acc (k, parts) -> do
              let ciKey = CaseInsensitive.mk (TE.encodeUtf8 k)
              rendered <- BEL.render env' (Aeson.String "") parts
              pure $ HM.insert ciKey rendered acc)
          HM.empty
          (HM.toList expectHeaders)

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
