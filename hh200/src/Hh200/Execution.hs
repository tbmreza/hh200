{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hh200.Execution
  ( testOutsideWorld
  , runProcM
  , assertsAreOk
  , ProcM
  , status200
  , testShotgun
  ) where

-- testShotgun chatgpt
import Control.Concurrent       (threadDelay)
import Control.Concurrent.QSemN
import Control.Exception        (bracket_, try)
import Control.Monad            (forM)
import Control.Concurrent.Async (mapConcurrently)
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Simple

import Hh200.Types
import Debug.Trace
import qualified Data.List.NonEmpty as Ls (NonEmpty(..))
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Char8 as BS
import qualified Data.CaseInsensitive as CaseInsensitive (mk)
import Network.HTTP.Client.TLS
import qualified Data.ByteString.Lazy.Char8 as L8

import qualified Network.HTTP.Client as Prim
  ( newManager, parseRequest, httpLbs, method, requestBody, requestHeaders, responseStatus, responseBody
  , Manager, Response, Request, RequestBody(..)
  )

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Maybe (fromJust)
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header (HeaderName)
import Control.Monad (foldM, forM)
import qualified Control.Monad.Trans.RWS.Strict as Tf

import qualified Data.Text as Text
import           Data.Text (Text)
import qualified Data.Aeson as Aeson (decode)
import qualified Data.Aeson.Types as Aeson (Value(..))
import qualified BEL

-- Procedure "may" fail early, "reads" a shared http-client manager instance,
-- "writes" log as it runs, modifies environment "states" while doing IO.
type ProcM = MaybeT (Tf.RWST Prim.Manager Log Env IO)

--------------------------------------------------------------------------------
-- Defaults & Smart Constructors
--------------------------------------------------------------------------------

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

nonLead :: Script -> Lead
nonLead x = NonLead
  { firstFailing = Nothing
  , hostInfo = defaultHostInfo
  , interpreterInfo = (HM.empty, [])
  , echoScript = Just x
  }

leadFrom :: Maybe CallItem -> (Env, Log) -> Script -> Lead
leadFrom failed el script = Lead
  { firstFailing = failed
  , hostInfo = defaultHostInfo
  , echoScript = Just script
  , interpreterInfo = el
  }

_debugLead :: Lead
_debugLead = DebugLead
  { firstFailing = Nothing
  , hostInfo = defaultHostInfo
  , echoScript = Nothing
  , interpreterInfo = (HM.empty, [])
  }

--------------------------------------------------------------------------------
-- Small Helpers / Utilities
--------------------------------------------------------------------------------

asMethod :: UppercaseString -> BS.ByteString
asMethod (UppercaseString s) = BS.pack s

validJsonBody :: Prim.Response L8.ByteString -> Aeson.Value
validJsonBody resp =
    case Aeson.decode (Prim.responseBody resp) of
        Just av -> trace ("validJsonBody:\t" ++ show av) av
        Nothing -> trace "validJsonBody NULL!!!" Aeson.Null

asBS :: Aeson.Value -> BS.ByteString
asBS _ = ""  -- ??

textOrMt :: Aeson.Value -> Text
textOrMt (Aeson.String t) = t
textOrMt _ = ""

stringOrMt :: Aeson.Value -> String
stringOrMt v = Text.unpack $ textOrMt v

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

headerJson :: (HeaderName, BS.ByteString)
headerJson = ("Content-Type", "application/json")

--------------------------------------------------------------------------------
-- Logic / Execution
--------------------------------------------------------------------------------

-- False indicates for corresponding CallItem (perhaps on user assert) to be reported.
assertsAreOk :: Env -> Prim.Response a -> Maybe ResponseSpec -> IO Bool
assertsAreOk env got mrs = do
    case elem (Prim.responseStatus got) (expectCodesOrDefault mrs) of
        False -> trace ("dis falz:" ++ show (Prim.responseStatus got) ++ "and:" ++ show (expectCodesOrDefault mrs)) $ pure False
        _ -> do
            values :: [Aeson.Value] <- mapM (BEL.eval env) linesOrMt
            pure $ notElem (Aeson.Bool False) values
    where
    linesOrMt :: [Text]
    linesOrMt = case mrs of
        Just rs -> map Text.pack $ asserts rs
        _ -> []

expectCodesOrDefault :: Maybe ResponseSpec -> [Status]
expectCodesOrDefault mrs =
    case mrs of
        Nothing -> [status200]
        Just rs -> case statuses rs of
            [] -> [status200]
            expectCodes -> expectCodes

-- Return to user the CallItem which we suspect will fail again.
runProcM :: Script -> Prim.Manager -> Env -> IO Lead
runProcM script mgr env = do
    results <- Tf.runRWST (runMaybeT $ courseFrom script) mgr env
    let (_mci, finalEnv, _log) = results
    -- pure (switch results)
    pure (switch (trace ("finalEnv:\n" ++ show finalEnv) $ results))

    where
    switch :: (Maybe CallItem, Env, Log) -> Lead
    switch (mci, e, l)
        | "default" == (ciName $ fromJust mci) =
            nonLead script
        | otherwise =
            leadFrom mci (e, l) script

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
    lift $ Tf.tell ["Processing script with " ++ show (length $ callItems x) ++ " call items..."]
    env <- get
    pairs <- liftIO (mapM (buildFrom env) (callItems x))
    liftIOWithMgr pairs

    where
    -- ??: work out why passing Env like this is correct/incorrect
    -- [X] dict passing
    -- [ ] concurrency
    -- Build Request and echo CallItem parts.
    buildFrom :: Env -> CallItem -> IO (Prim.Request, (CallItem, Maybe ResponseSpec))
    buildFrom env ci
        -- Requests without body.
        | null (payload $ ciRequestSpec ci) = do
            struct <- parseUrl

            renderedHeaders <- renderHeaders (headers $ ciRequestSpec ci)
            -- renderedHeaders <- do
            --     let (RhsDict unrenderedHeaders) = (headers $ ciRequestSpec ci)
            --     pure [("user-agent", "quinlanarcher@gmail.com")]

            dorp (ci, (ciResponseSpec ci)) struct
              { Prim.method = asMethod (verb $ ciRequestSpec ci)
              , Prim.requestHeaders = renderedHeaders
              }

        -- Requests with json body.
        | otherwise = do
            struct <- parseUrl
            rb <- stringRender (payload $ ciRequestSpec ci)

            dorp (ci, (ciResponseSpec ci)) struct
              { Prim.method = asMethod (verb $ ciRequestSpec ci)
              , Prim.requestHeaders = [headerJson]
              , Prim.requestBody = trace ("rawPayload\t" ++ rb ++ ";") (rawPayload rb)
              }

        where
        renderHeaders :: RhsDict -> IO [(HeaderName, BS.ByteString)]
        renderHeaders (RhsDict bindings) = do
            traverseKV bindings $
                \(k :: String) (v :: [BEL.Part]) -> do
                    av <- BEL.render env (Aeson.String "") v
                    pure (CaseInsensitive.mk (BS.pack k), asBS av)

        parseUrl :: IO Prim.Request
        parseUrl = do
            rendered <- stringRender (url $ ciRequestSpec ci)
            Prim.parseRequest rendered

        -- Return swapped product (`dorp` is prod reversed).
        dorp :: a -> b -> IO (b, a)
        dorp a b = pure (b, a)

        stringRender :: String -> IO String
        stringRender s = do
            -- ??: this not a proper use of render; come back after unittesting rendering of mustached urls
            rendered :: Aeson.Value <- BEL.render env (Aeson.String "") (BEL.partitions $ Text.pack s)
            -- trace ("s\t" ++ s ++ ";\n" ++ "rendered\t" ++ show rendered ++ ";") (pure $ stringOrMt rendered)
            pure $ stringOrMt rendered

        rawPayload :: String -> Prim.RequestBody
        rawPayload s = Prim.RequestBodyLBS $ L8.pack s

    -- Results arrive here!
    liftIOWithMgr :: [(Prim.Request, (CallItem, Maybe ResponseSpec))] -> ProcM CallItem
    liftIOWithMgr pairs = do
        mgr :: Prim.Manager <- ask
        h mgr pairs

        where
        -- Reduce captures to Env extensions.
        evalCaptures :: Prim.Response L8.ByteString
                     -> (Env, Maybe ResponseSpec)
                     -> IO (Env -> Env, Log)

        evalCaptures resp (env, mrs) = do
            let (RhsDict bindings) = case mrs of
                    Nothing -> RhsDict HM.empty
                    Just rs -> captures rs

            let initialLog = if HM.null bindings then [] else ["Processing " ++ show (HM.size bindings) ++ " captures..."]

            (ext, finalLog) <- foldlWithKeyM'
                (\(acc, logs) bK (bV :: [BEL.Part]) -> do
                    v <- BEL.render acc (Aeson.String "") bV
                    pure (HM.insert bK v acc, logs ++ ["  Captured `" ++ bK ++ "`"])
                )
                (HM.insert "RESP_BODY" (validJsonBody resp) env, initialLog)
                bindings

            pure (const ext, finalLog)

        -- response = {captures, asserts}. request = {configs ("options" in hurl), cookies}
        h :: Prim.Manager -> [(Prim.Request, (CallItem, Maybe ResponseSpec))] -> ProcM CallItem
        h mgr list = case list of
                [] -> emptyHanded

                (req, (ci, mrs)) : rest -> do
                    lift $ Tf.tell ["-- Executing call item `" ++ ciName ci ++ "`"]
                    -- Unhandled offline HttpExceptionRequest.
                    eitherResp <- liftIO ((try (Prim.httpLbs req mgr)) :: IO (Either HttpException (Prim.Response L8.ByteString)))
                    case eitherResp of
                      Left e -> do
                        lift $ Tf.tell ["HTTP request failed: " ++ show e]
                        pure ci
                      Right gotResp -> do
                        lift $ Tf.tell ["Request completed with status: " ++ show (Prim.responseStatus gotResp)]
                        -- Captures.
                        env <- get

                        (upsertCaptures, captureLog) <- liftIO (evalCaptures gotResp (env, mrs))
                        lift $ Tf.tell captureLog

                        -- Unless null Captures:
                        modify upsertCaptures

                        res <- liftIO $ assertsAreOk env gotResp mrs
                        case res of
                            False -> do
                                lift $ Tf.tell ["Assertions failed."]
                                pure ci
                            _ -> do
                                lift $ Tf.tell ["Assertions passed."]
                                h mgr rest

testOutsideWorld :: Script -> IO Lead

-- -> NonLead
testOutsideWorld static@(Script { config = _, callItems = [] }) = do
    pure $ nonLead static

-- -> NonLead | DebugLead | Lead
testOutsideWorld sole@(
    Script
      { config = ScriptConfig { subjects = _ }
      , callItems = [_] }) = do
    mgr <- Prim.newManager tlsManagerSettings
    let envNew :: Env = HM.fromList
            [ ("yyyymmdd", Aeson.String "19700101")
            , ("undefined", Aeson.String "falsy")
            ]
    runProcM sole mgr envNew

-- -> NonLead | DebugLead | Lead
testOutsideWorld flow@(Script { callItems = _ }) = do
    mgr <- Prim.newManager tlsManagerSettings
    runProcM flow mgr HM.empty

testOutsideWorld unexpected = do
    pure $ nonLead unexpected


-- -- counter-example presentation based on mvar
-- -- in the normal testOutsideWorld-present combo
-- -- Script { callItems = HM.HashMap name [CallItem] }
--
-- -- duration-bound virtual user we name rps (in reference to locust RPS)
-- -- integrates with vscode test runner
-- testRps :: Minutes -> Script -> IO ()
-- testOutsideWorld & testShotgun grow hand in hand, while testRps starts
-- a web js server.
testRps :: IO ()
testRps = pure ()

-- -- thread-based parallelism we name shotgun: based on async + QSemN
-- Thread-based parallelism based on async and QSemN.
-- Unlike testOutsideWorld, testShotgun is aware of what the (usually two) variables are.
-- Almost like testOutsideWorld can step in a running testShotgun and contribute one number.
-- testOutsideWorld (pre alpha) appends Pct to output/history.dat

-- Limit concurrency with QSemN
mapConcurrentlyBounded :: Int -> [IO a] -> IO [a]
mapConcurrentlyBounded n actions = do
    sem <- newQSemN n
    mapConcurrently
        (\act -> bracket_ (waitQSemN sem 1)
                          (signalQSemN sem 1)
                          act)
        actions

-- One HTTP request
fetch :: Int -> IO BL.ByteString
fetch i = do
    putStrLn $ "Starting request " ++ show i
    response <- httpLBS "https://httpbin.org/delay/1"
    putStrLn $ "Finished request " ++ show i
    pure (getResponseBody response)

testShotgun :: Int -> Script -> IO Lead
testShotgun n checked = do
    mgr <- Prim.newManager tlsManagerSettings
    -- let jobs :: [IO L8.ByteString] = map fetch [1..2]

    putStrLn "Running HTTP calls with n parallel workers…"
    results <- mapConcurrentlyBounded n [runProcM checked mgr HM.empty, runProcM checked mgr HM.empty, runProcM checked mgr HM.empty, runProcM checked mgr HM.empty]

    putStrLn $ "Done. Got " ++ show (length results) ++ " responses."

    runProcM checked mgr HM.empty


-- ??: visualize this in 2D gp table
data DataPoint = DataPoint
  { shotgunN :: Int
  , shotgunPct :: Double  -- Percentage of user assertions satisfying responses.
  }
mkDataPoint :: Int -> DataPoint
mkDataPoint n = DataPoint { shotgunN = n, shotgunPct = 0.0 }
