{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

-- | TokenBucketWorkerPool uses this module to execute Scripts.
module Hh200.Execution
  ( runScriptM

  -- , runProcM
  -- , conduct
  , ProcM
  , status200
  , renderHeadersMap
  , renderRequestQuery
  -- , renderRequestUrl
  , renderRequestForm
  , renderRequestCookies
  , objectSubset
  , jsonSubset
  , SubsetResult(..)
  , Side(..)
  , experimentalRequestBodyFile'
  ) where

import Debug.Trace

import           System.IO (hPutStrLn, stderr, stdout, hClose, openFile, IOMode(..))

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.QSemN
import           Control.Exception        (bracket, bracket_, try, SomeException, IOException)
import           Control.Concurrent.Async (mapConcurrently, replicateConcurrently_)
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Except (runExceptT, ExceptT(..))
import           Control.Monad (foldM, forM, mzero, forever, void)
import           Control.Monad (forM_, replicateM, replicateM_, when)
import qualified Control.Monad.Trans.RWS.Strict as Tf

import           Data.Traversable (for)
import           Data.Maybe (fromMaybe)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Char8 as BS
import qualified Data.CaseInsensitive as CaseInsensitive
import qualified Data.ByteString.Lazy.Char8 as L8
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
                                           , RequestBody (RequestBodyBS, RequestBodyLBS)
                                           , streamFile
                                           )
import qualified Network.HTTP.Client.MultipartFormData as HCMP
import           Network.HTTP.Client.MultipartFormData (partFileSource, formDataBody)
import           Network.HTTP.Types.Status
import           Network.HTTP.Types.Header (HeaderName, ResponseHeaders)
import           Network.HTTP.Simple (setRequestBodyFile, setRequestBody, setRequestBodyJSON)
import qualified Network.HTTP.Client.Internal as HI

import qualified BEL
import qualified Hh200.Http as Http
import           Hh200.Types
import           Hh200.Graph (connect)
import           Hh200.Scanner (gatherHostInfo)

-- experimentalRequestBodyFile :: Int
experimentalRequestBodyFile = setRequestBody . HI.RequestBodyIO . HC.streamFile

-- Set request file body, or no-op if something unexpected happened.
experimentalRequestBodyFile' :: FilePath -> HI.Request -> IO HI.Request
experimentalRequestBodyFile' path req = do
    result <- try @IOException $ do
        handle <- openFile path ReadMode
        hClose handle  -- (auto) just a probe; streamFile will reopen
    pure $ case result of
        Left err -> req
        Right _  -> setRequestBody (HI.RequestBodyIO (HC.streamFile path)) req

-- (auto)
requestBodyMultipart :: [(Text, FilePath)] -> HI.Request -> IO HI.Request
requestBodyMultipart fileParts req = do
    -- let parts = [ partFileSource (TE.encodeUtf8 fieldName) fp
    let parts = [ partFileSource fieldName fp
                | (fieldName, fp) <- fileParts ]
    formDataBody parts req
    -- formDataBody sets Content-Type: multipart/form-data; boundary=...
    -- and Content-Length automatically

data BodyPart =
    BodyPartFile { bpField :: Text, bpPath :: FilePath }
  | BodyPartText  { bpField :: Text, bpValue :: Text    }
    deriving (Show)

type HhValue = Int
data HhRequestBody =
    RBJson        HhValue
  | RBFormUrl     [(Text, Text)]
  | RBMultipart   [BodyPart]          -- new
  | RBRaw         BS.ByteString Text  -- raw body + content-type

bodyPartToPart :: BodyPart -> HCMP.Part
bodyPartToPart (BodyPartFile field fp) = partFileSource field fp
bodyPartToPart (BodyPartText field val) = HCMP.partBS field (TE.encodeUtf8 val)

applyBody :: HhRequestBody -> HI.Request -> IO HI.Request
applyBody (RBMultipart parts) req =
    formDataBody (map bodyPartToPart parts) req

applyBody (RBJson v) req =
    pure $ setRequestBodyJSON v req
applyBody _ _ = undefined


-- experimentalRequestBodyFile'' :: FilePath -> H.Request -> IO (Either IOException H.Request)
experimentalRequestBodyFile'' path req = runExceptT $ do
  bytes <- ExceptT $ try @IOException (BS.readFile path)
  pure $ setRequestBody (HI.RequestBodyBS bytes) req

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
        (_,       Nothing) -> InvalidJson SideB
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

-- Procedure "may" fail early, "reads" an execution context,
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

headersToAeson :: [(HeaderName, BS.ByteString)] -> Aeson.Value
headersToAeson hdrs = Aeson.Object $ KeyMap.fromList $ 
    map (\(k, v) -> (Key.fromText (Text.pack (BS.unpack (CaseInsensitive.original k))), Aeson.String (TE.decodeUtf8With TEE.lenientDecode v))) hdrs

asBS :: Aeson.Value -> BS.ByteString
asBS (Aeson.String t) = TE.encodeUtf8 t
asBS v                = BL.toStrict (Aeson.encode v)

runScriptM :: Script -> Env -> IO ()
runScriptM script env = do
    let course :: ProcM CallItem = courseFrom script
    mgr <- Http.newManager True
    _ <- Tf.runRWST (runMaybeT course) mgr env
    pure ()

-- -- ??: to be seen if we need to abstract Manager or not
-- runProcM :: Script -> Http.Manager -> Env -> IO (Maybe CallItem, Env, Log)
-- runProcM script mgr env = do
--     let course :: ProcM CallItem = courseFrom script
--     Tf.runRWST (runMaybeT course) mgr env
--
-- -- ??: waiting for stable worker/tbwp
-- conduct :: Script -> Http.Manager -> Env -> IO Lead
-- conduct script mgr env = do
--     hi <- gatherHostInfo
--     result <- try (runProcM script mgr env)
--     case result of
--         Left (e :: SomeException) -> do
--             let errLog = [HttpError (show e)]
--             -- We use defaultCallItem to indicate a system/execution error
--             -- but we might want a more specific "Error" CallItem later.
--             pure $ leadFrom (Just defaultCallItem) (env, errLog) script hi
--         Right (mci, finalEnv, procLog) -> do
--             traceM $ "Execution finished. Log length: " ++ show (length procLog)
--             pure $ leadFrom mci (finalEnv, procLog) script hi
--
--     where
--     leadFrom :: Maybe CallItem -> (Env, Log) -> Script -> HostInfo -> Lead
--     leadFrom failed el script hi = Lead
--       { leadKind = Normal
--       , firstFailing = failed
--       , hostInfo = hi
--       , echoScript = Just script
--       , interpreterInfo = el
--       }
-- testOutsideWorld :: Script -> IO Lead
-- testOutsideWorld script = do
--     bracket (Http.newManager True)
--             Http.closeManager $
--             \mgr -> conduct script mgr HM.empty


specCapturesOrMt :: CallItem -> RhsDict
specCapturesOrMt ci =
    case ciResponseSpec ci of
        Nothing -> mtRhsDict
        Just rp ->
            case rpSquares rp of
                (Just (ResponseSquareCaptures d), _) -> d
                _ -> mtRhsDict

specCodesOr200 :: CallItem -> [Status]
specCodesOr200 ci =
    case ciResponseSpec ci of
        Nothing -> [status200]
        Just rs -> case rpStatuses rs of
            [] -> [status200]
            expectCodes -> expectCodes

specResponseBody :: CallItem -> String
specResponseBody ci =
    case ciResponseSpec ci of
        Nothing -> ""
        Just rs -> rpBody rs

specAssertionsOrMt :: CallItem -> [Text]
specAssertionsOrMt ci =
    case ciResponseSpec ci of
        Nothing -> []
        Just rp ->
            case rpSquares rp of
                (_, Just (ResponseSquareAsserts d)) -> map Text.pack d
                _ -> []

isHhFilePrefix :: Text -> Bool
isHhFilePrefix _ = False

-- goal in order: multipartSq, rqUrl, test braced interpolation
buildRequest :: Env -> CallItem -> IO Http.Request
buildRequest env CallItem { ciRequestSpec = RequestSpec { rqMethod
                                                        , rqUrl
                                                        , rqHeaders = RhsDict dHeaders
                                                        , rqBody
                                                        , rqSquares = (configsSq, querySq, formSq, multipartSq, cookiesSq) } } = do
    -- rqUrl, rqHeaders, rqBody interpolatable

    eHeaders <- renderRqHeaders
    -- cookiesHeaders <- renderRequestCookies env cookiesSq
    -- renderedQuery <-  renderRequestQuery env querySq
    eUrl <- renderRqUrl

    -- req <- HC.parseRequest eUrl
    -- req :: HI.Request <- HC.parseRequest ""
    req :: HI.Request <- HC.parseRequest eUrl

    -- let allHeaders = eHeaders ++ cookiesHeaders
    let allHeaders = eHeaders

    -- renderMultipartb req allHeaders ""

    handleMultipartElems req

    where
    -- fin :: Http.Request -> IO Http.Request
    -- fin req = do
    --     let cpy = env
    --     pure req

    renderRqUrl :: IO String
    renderRqUrl = do
        renderedQuery <- renderRequestQuery env querySq
        renderRequestUrlHelper rqUrl renderedQuery

    renderRequestUrlHelper :: LexedUrl -> String -> IO String
    renderRequestUrlHelper (LexedUrlFull s) renderedQuery = pure $ case renderedQuery of
        "" -> s
        qs -> s ++ "?" ++ qs
    renderRequestUrlHelper (LexedUrlSegments urlParts) renderedQuery = do
        renderedUrlParts <- forM urlParts $ \part -> do
            rendered <- BEL.render env (Aeson.String "") [part]
            pure $ case rendered of
                Aeson.String t -> Text.unpack t
                v -> BS.unpack (BL.toStrict (Aeson.encode v))
        let fullUrl = intercalate "/" renderedUrlParts
        pure $ case renderedQuery of
            "" -> fullUrl
            qs -> fullUrl ++ "?" ++ qs

    renderRqHeaders :: IO [(HeaderName, BS.ByteString)]
    renderRqHeaders = for (HM.toList dHeaders)
        (\ (k, v :: [BEL.Part]) -> do
            e <- renderOrEmpty env v
            pure (asHeaderName k, TE.encodeUtf8 e))

    -- Handle functions are for specs that warrant direct fallible struct modifications.
    -- Handle functions are for specs with logical dependencies.
    -- Handle functions are for specs with logical dependencies and warrant direct fallible struct modifications.

    handleMultipartElems :: HI.Request -> IO HI.Request
    handleMultipartElems req = do
    -- PICKUP try idea of auto
    -- ??: amalgamate [FilePath] for multiple files
        case multipartSq of
            Just (RequestSquareMultipart m@(RhsDict d)) -> do
                -- eMultipart <- hmeRender m
                -- let eMultipart = 
                --
                -- 1. { "file": filepath }

                -- eMultipart <- for (HM.toList d)
                --     (\ (k, v :: [BEL.Part]) -> do
                --         e <- renderOrEmpty env v
                --         -- isHhFilePrefix e
                --         trace ("renderOrEmpty:" ++ show e) 9)
                let filepathV = "/home/tbmreza/gh/hh200/hh200/target-template.xls"
                -- req' <- experimentalRequestBodyFile' filepathV req
                req' <- requestBodyMultipart [] req
                let bod :: BodyPart = BodyPartFile { bpField = "file", bpPath = filepathV }
                let eMultipart :: HhRequestBody = RBMultipart [bod]
                got <- applyBody eMultipart req
                pure $ got { HC.method = BS.pack rqMethod
                           -- , 
                           }

                -- let contentType = "multipart/form-data; boundary=--------------------------414973037462257374369442"
                -- -- let zz :: [(CaseInsensitive.CI BS.ByteString, BS.ByteString)] = (CaseInsensitive.mk "Content-Type", BS.pack contentType) : allHeaders
                -- let zz :: [(CaseInsensitive.CI BS.ByteString, BS.ByteString)] = (CaseInsensitive.mk "Content-Type", BS.pack contentType) : []
                -- pure $ req' { HC.method = BS.pack rqMethod
                --             , HC.requestHeaders = zz
                --             -- , HC.requestHeaders = (CaseInsensitive.mk "Content-Type", BS.pack contentType)
                --             -- , HC.requestHeaders = (CaseInsensitive.mk "Content-Type", BS.pack contentType) : allHeaders
                --             }

                -- 2. { "kkk": 14 }
            _ -> pure req

    -- hme ::

    -- hmeRender :: RhsDict -> IO (Text, String)
    hmeRender (RhsDict dMultipart) = for (HM.toList dMultipart)
        (\ (k, v :: [BEL.Part]) -> do
            -- e <- renderOrEmpty env v
            -- pure undefined)
            -- pure (k, "strin"))
            pure (undefined, undefined))


    -- Assumption: File reading is only needed for multipart so it acts as
    -- req struct finalizer.
    -- renderMultipartb :: Env -> Http.Request -> [(HeaderName, BS.ByteString)] -> Maybe RequestSquare -> String -> String -> IO Http.Request
    renderMultipartb :: Http.Request -> [(HeaderName, BS.ByteString)] -> String -> IO Http.Request
    renderMultipartb req allHeaders renderedForm =
        case multipartSq of
            Just sq -> do
                -- case multipartFilepathAt sq of
                --     Just f -> undefined
                --     _ -> undefined

                -- multipartFilepathAt sq
                req' <- experimentalRequestBodyFile' "" req
                let bodyContent = case (renderedForm, rqBody) of
                        ("", "") -> BS.pack rqBody
                        ("", _) -> BS.pack rqBody
                        (f, "") -> BS.pack f
                        (f, _) -> BS.pack f
                    contentType = if null renderedForm then "text/plain" else "application/x-www-form-urlencoded"  -- ??:
                    encoded = BL.fromStrict bodyContent
                let zz :: [(CaseInsensitive.CI BS.ByteString, BS.ByteString)] = (CaseInsensitive.mk "Content-Type", BS.pack contentType) : allHeaders
                pure $ req { HC.method = BS.pack rqMethod
                           , HC.requestHeaders = (CaseInsensitive.mk "Content-Type", BS.pack contentType) : allHeaders
                           , HC.requestBody = HC.RequestBodyLBS encoded
                           }

    -- renderRequestUrl :: BEL.Env -> LexedUrl -> String -> IO String
    renderRequestUrl :: String -> IO String
    renderRequestUrl renderedQuery = case rqUrl of
        LexedUrlFull s -> pure $ case renderedQuery of
            "" -> s
            qs -> s ++ "?" ++ qs
        LexedUrlSegments urlParts -> do
            renderedUrlParts <- forM urlParts $ \part -> do
                rendered <- BEL.render env (Aeson.String "") [part]
                pure $ case rendered of
                    Aeson.String t -> Text.unpack t
                    v -> BS.unpack (BL.toStrict (Aeson.encode v))
            let fullUrl = intercalate "/" renderedUrlParts
            pure $ case renderedQuery of
                "" -> fullUrl
                qs -> fullUrl ++ "?" ++ qs

-- Exceptions:  when running ProcM
-- offline HttpExceptionRequest  -handling->  print
-- http client lib internal error  -handling->  halt (graceful if free)
courseFrom :: Script -> ProcM CallItem
courseFrom x = do
    lift $ Tf.tell [ScriptStart (length $ callItems x)]
    mgr <- ask
    go mgr (callItems x)

    where  -- goal in order: multipartSq, rqUrl, test braced interpolation
    -- renderMultipart :: Env -> Http.Request -> String -> [(HeaderName, BS.ByteString)] -> Maybe RequestSquare -> String -> String -> IO Http.Request
    -- renderMultipart env req rqMethod allHeaders multipartSq renderedForm rqBody =
    --     case multipartSq of
    --         -- Just (RequestSquareMultipart (RhsDict mpFields)) -> do
    --         --     boundary <- HCMP.webkitBoundary
    --         --
    --         --     parts <- forM (HM.toList mpFields) $ \ (k, parts') -> do
    --         --         rendered <- BEL.render env (Aeson.String "") parts'
    --         --         let bsValue = case rendered of
    --         --                 Aeson.String t -> TE.encodeUtf8 t
    --         --                 v -> BL.toStrict (Aeson.encode v)
    --         --         pure $ HCMP.partBS k (trace ("bsValue=" ++ show bsValue ++ "k=" ++ show k) bsValue)
    --         --     mpBody <- HCMP.renderParts boundary parts
    --         --     let contentType = BS.pack $ "multipart/form-data; boundary=" ++ BS.unpack boundary
    --         --     let req' = req { HC.method = BS.pack rqMethod
    --         --                    , HC.requestHeaders = (CaseInsensitive.mk "Content-Type", contentType) : allHeaders
    --         --                    , HC.requestBody = mpBody
    --         --                    }
    --         --     -- `setRequestBodyFile` upserts "accept-encoding", "content-length".
    --         --     let fullPath = "/home/tbmreza/gh/hh200/hh200/target-template.xls"
    --         --
    --         --     pure $ experimentalRequestBodyFile fullPath req'

    finalizeRequest :: Env -> Http.Request -> String -> [(HeaderName, BS.ByteString)] -> Maybe RequestSquare -> String -> String -> IO Http.Request
    finalizeRequest env req rqMethod allHeaders multipartSq renderedForm rqBody =
        case multipartSq of
            Just (RequestSquareMultipart (RhsDict mpFields)) -> do
                boundary <- HCMP.webkitBoundary
                -- field2: file,example.txt;
                -- field3: file,example.zip; application/zip
                parts <- forM (HM.toList mpFields) $ \ (k, parts') -> do
                    rendered <- BEL.render env (Aeson.String "") parts'
                    let bsValue = case rendered of
                            Aeson.String t -> TE.encodeUtf8 t
                            v -> BL.toStrict (Aeson.encode v)
                    pure $ HCMP.partBS k (trace ("bsValue=" ++ show bsValue ++ "k=" ++ show k) bsValue)
                mpBody <- HCMP.renderParts boundary parts
                let contentType = BS.pack $ "multipart/form-data; boundary=" ++ BS.unpack boundary
                let req' = req { HC.method = BS.pack rqMethod
                               , HC.requestHeaders = (CaseInsensitive.mk "Content-Type", contentType) : allHeaders
                               , HC.requestBody = mpBody
                               }
                -- `setRequestBodyFile` upserts "accept-encoding", "content-length".
                let fullPath = "/home/tbmreza/gh/hh200/hh200/target-template.xls"

                -- pure $ setRequestBodyFile fullPath req'
                pure $ experimentalRequestBodyFile fullPath req'
                -- trace ("inikan" ++ "mpFields=" ++ show mpFields) $ pure $ experimentalRequestBodyFile' fullPath req'  -- Expected: IO Http.Request Actual: IO (IO (Either IOException HI.Request))
                -- trace ("inikan" ++ "mpFields=" ++ show mpFields) $ pure $ experimentalRequestBodyFile'' fullPath req'  -- Expected: IO Http.Request Actual: IO (IO (Either IOException HI.Request))
            _ -> do
                let bodyContent = case (renderedForm, rqBody) of
                        ("", "") -> BS.pack rqBody
                        ("", _) -> BS.pack rqBody
                        (f, "") -> BS.pack f
                        (f, _) -> BS.pack f
                    contentType = if null renderedForm then "text/plain" else "application/x-www-form-urlencoded"
                    encoded = BL.fromStrict bodyContent
                pure $ req { HC.method = BS.pack rqMethod
                           , HC.requestHeaders = (CaseInsensitive.mk "Content-Type", BS.pack contentType) : allHeaders
                           , HC.requestBody = HC.RequestBodyLBS (trace ("encoded=" ++ show encoded) encoded)
                           }

    -- go :: Http.Manager -> [CallItem] -> ProcM CallItem
    go _ [] = mzero
    go mgr (ci:rest) = do
    -- go [] = mzero
    -- go (ci:rest) = do
        lift $ Tf.tell [ItemStart (ciName ci)]

        env <- get
        reqOrThrow <- liftIO $ buildRequest env ci
        -- reqOrThrow <- liftIO $ buildRequest' env ci

        -- ??: after exception handling sites are clear, print offline HttpExceptionRequest to user right away (or else).
        -- eitherResp <- liftIO ((try (Http.httpLbs reqOrThrow mgr)) :: IO (Either Http.HttpException Http.Response))
        -- ??: handle multipart file not found
        eitherResp <- liftIO ((try (Http.httpLbs reqOrThrow mgr)) :: IO (Either Http.HttpException Http.Response))
        _ <- liftIO $ putStrLn $ present ci  -- ??: only failing ci
        case eitherResp of
            Left e -> do
                -- https://hackage-content.haskell.org/package/http-client-0.7.19/docs/src/Network.HTTP.Client.Types.html#HttpException

                lift $ Tf.tell [HttpError (show e)]
                pure ci
            Right gotResp -> do
                let envWithResp = env { BEL.storedResponse = gotResp
                                      , BEL.storedRequest = reqOrThrow
                                      }
                f <- liftIO (upsertCaptures envWithResp ci)
                modify f

                env' <- get

                ok <- liftIO (userAssertions env' ci)
                if not ok then
                    pure ci
                else
                    go mgr rest
                    -- go rest

    -- Status code assertion first, then all other checks (headers, body, and
    -- expressions about the response).
    userAssertions :: BEL.Env -> CallItem -> IO Bool
    userAssertions env' ci = do
        let expectCodes = specCodesOr200 ci
            -- gotResp :: HC.Response L8.ByteString = storedResponse env'
            gotResp :: HC.Response L8.ByteString = BEL.storedResponse env'
            gotStatus = Http.getStatus gotResp

        if gotStatus `notElem` expectCodes then
            failWith ("status=" ++ show gotStatus ++ ", expect=" ++ show
                      expectCodes)
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
            -- Default: assert subset of actual response body if it's json.
            -------------------------------------------------------------------

            let expectRespBody :: L8.ByteString = L8.pack $ specResponseBody ci
            let gotRespBody :: L8.ByteString = HC.responseBody gotResp
            let completeCheckedJsonBody =
                    let expectedBs = BL.toStrict expectRespBody
                        gotBs = BL.toStrict gotRespBody
                    in case (expectedBs, jsonSubset (trace ("expectedBs=" ++ show expectedBs) expectedBs) gotBs) of
                        ("", _) -> trace "jsonSubset skip" True
                        (v, ASubsetOfB) -> trace "jsonSubset true" True
                        (_, v) -> trace ("jsonSubset=" ++ show v) False


            -------------------------------------------------------------------
            -- Collect [Asserts] expressions checks.
            --
            -- BEL evaluates all lines at once (for desired effect of visible
            -- BEL prints), but single False indicates for the whole [Asserts]
            -- block to be failing.
            -------------------------------------------------------------------
            expressions <- BEL.mapEval env' (specAssertionsOrMt ci)
            let aesonValues = map BEL.finalValue expressions

            pure $ and [ Aeson.Bool False `notElem` aesonValues
                       , completeCheckedHeaders
                       , completeCheckedJsonBody
                       ]

    -- Reduce captures to Env extensions.
    upsertCaptures :: BEL.Env -> CallItem -> IO (b0 -> BEL.Env)
    upsertCaptures env' ci = do
        let RhsDict c = specCapturesOrMt ci

        ext <- foldlWithKeyM'
            (\ acc bK (bV :: [BEL.Part]) -> do
                v <- BEL.render acc (Aeson.String "") bV
                pure (acc { BEL.bindings = HM.insert bK v (BEL.bindings acc) }))
            env'
            c

        pure (const ext)

failWith :: String -> IO Bool
failWith msg = hPutStrLn stderr msg >> pure False


triggerEmergencyShutdown :: TVar Bool -> IO ()
triggerEmergencyShutdown flag = do
    putStrLn "🚨 EMERGENCY SHUTDOWN TRIGGERED"
    atomically $ writeTVar flag True

asHeaderName :: Text -> HeaderName
asHeaderName t = CaseInsensitive.mk (TE.encodeUtf8 t)

-- collectHeaders
-- collectMultipart
-- renderRhsDict :: BEL.Env -> RhsDict -> IO [(BS.ByteString, BS.ByteString)]
renderRhsDict :: BEL.Env -> RhsDict -> IO [(HeaderName, BS.ByteString)]
renderRhsDict env (RhsDict dict) = do
    results <- HM.traverseWithKey (renderHeader env) dict
    let toByteStringPairs = map (\(k, v) -> (CaseInsensitive.mk (TE.encodeUtf8 k), v))
    pure $ toByteStringPairs $ HM.toList results

    where
    renderHeader :: BEL.Env -> Text -> [BEL.Part] -> IO BS.ByteString
    renderHeader env _ parts = do
        val <- BEL.render env (Aeson.String "") parts
        pure $ case val of
            Aeson.String s -> TE.encodeUtf8 s
            _ -> TE.encodeUtf8 (Text.pack $ show val)

-- ??: BEL repurpose an Aeson value to encode "filepath to read"
-- renderOrMt :: BEL.Env -> Text -> [BEL.Part] -> IO Text
-- renderOrMt env _k parts = do
--     av <- BEL.render env (Aeson.String "") parts
--     pure $ case av of
--         Aeson.String s -> s
--         _ -> error "shouldn't have happened"

renderOrEmpty :: BEL.Env -> [BEL.Part] -> IO Text
renderOrEmpty env parts = do
    av <- BEL.render env (Aeson.String "") parts
    pure $ case av of
        Aeson.String s -> s
        _ -> error "shouldn't have happened"

-- renderMultipart :: BEL.Env -> RhsDict -> IO [(HeaderName, BS.ByteString)]
renderMultipart env multipart@(RhsDict dict) = do
    -- dict' <- HM.traverseWithKey (renderOrMt env) dict
    undefined


-- | Render expected response headers.
renderHeadersMap :: BEL.Env -> RhsDict
                 -> IO (HM.HashMap (CaseInsensitive.CI BS.ByteString) Aeson.Value)
renderHeadersMap env (RhsDict expectHeaders) =
    foldM (\ acc (k, parts) -> do
               rendered <- BEL.render env (Aeson.String "") parts
               let ciKey = CaseInsensitive.mk (TE.encodeUtf8 k)
               pure $ HM.insert ciKey rendered acc )
          HM.empty
          (HM.toList expectHeaders)

renderRequestConfigs :: BEL.Env -> Maybe RequestSquare -> IO [(BS.ByteString, BS.ByteString)]
renderRequestConfigs _ Nothing = pure []
renderRequestConfigs _ (Just (RequestSquareConfigs _)) = pure []

renderRequestQuery :: BEL.Env -> Maybe RequestSquare -> IO String
renderRequestQuery _ Nothing = pure ""
renderRequestQuery env' (Just (RequestSquareQuery (RhsDict qp))) = do
    pairs <- forM (HM.toList qp) $ \ (k, parts) -> do
        rendered <- BEL.render env' (Aeson.String "") parts
        -- ??: pattern matching might not be necessary if BEL.render Aeson output is stable
        let bsValue = case rendered of
                Aeson.String t -> TE.encodeUtf8 t
                v -> BL.toStrict (Aeson.encode v)
        pure (Text.unpack k ++ "=" ++ BS.unpack bsValue)
    pure $ intercalate "&" pairs

renderRequestForm :: BEL.Env -> Maybe RequestSquare -> IO String
renderRequestForm _ Nothing = pure ""
renderRequestForm env' (Just (RequestSquareForm (RhsDict formFields))) = do
    pairs <- forM (HM.toList formFields) $ \ (k, parts) -> do
        rendered <- BEL.render env' (Aeson.String "") parts
        let bsValue = case rendered of
                Aeson.String t -> TE.encodeUtf8 t
                v -> BL.toStrict (Aeson.encode v)
        pure (Text.unpack k ++ "=" ++ BS.unpack bsValue)
    pure $ intercalate "&" pairs

-- renderRequestUrl :: BEL.Env -> LexedUrl -> String -> IO String
-- renderRequestUrl env rqUrl renderedQuery = case rqUrl of
--     LexedUrlFull s -> pure $ case renderedQuery of
--         "" -> s
--         qs -> s ++ "?" ++ qs
--     LexedUrlSegments urlParts -> do
--         renderedUrlParts <- forM urlParts $ \part -> do
--             rendered <- BEL.render env (Aeson.String "") [part]
--             pure $ case rendered of
--                 Aeson.String t -> Text.unpack t
--                 v -> BS.unpack (BL.toStrict (Aeson.encode v))
--         let fullUrl = intercalate "/" renderedUrlParts
--         pure $ case renderedQuery of
--             "" -> fullUrl
--             qs -> fullUrl ++ "?" ++ qs

renderRequestCookies :: BEL.Env -> Maybe RequestSquare -> IO [(HeaderName, BS.ByteString)]
renderRequestCookies _ Nothing = pure []
renderRequestCookies env' (Just (RequestSquareCookies (RhsDict ck))) = do
    pairs <- forM (HM.toList ck) $ \ (k, parts) -> do
        rendered <- BEL.render env' (Aeson.String "") parts
        let bsValue = case rendered of
                Aeson.String t -> TE.encodeUtf8 t
                v -> BL.toStrict (Aeson.encode v)
        pure (Text.unpack k ++ "=" ++ BS.unpack bsValue)
    let cookieHeader = BS.pack $ intercalate "; " pairs
    pure [(CaseInsensitive.mk "Cookie", cookieHeader)]

--------------------------------------------------------------------------------
-- More lib than app code
--------------------------------------------------------------------------------
intercalate :: String -> [String] -> String
intercalate _ [] = ""
intercalate _ [x] = x
intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs

foldlWithKeyM' :: (Monad m) => (a -> k -> v -> m a)
                            -> a
                            -> HM.HashMap k v
                            -> m a
foldlWithKeyM' f z0 hm = foldM step z0 (HM.toList hm)
    where
    step !acc (k, v) = f acc k v

-- traverseKV :: HM.HashMap k v -> (k -> v -> IO a) -> IO [a]
-- traverseKV hm f =
--     forM (HM.toList hm) $ \(k, v) -> f k v
