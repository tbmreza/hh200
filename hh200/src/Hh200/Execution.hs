{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hh200.Execution
  ( testOutsideWorld , testShotgun, testRps
  , runProcM
  , assertsAreOk
  , ProcM
  , status200
  ) where

import Control.Concurrent       (threadDelay)
import Control.Concurrent.QSemN
import Control.Exception        (bracket, bracket_, try)
import Control.Monad            (forM)

import qualified Data.Text.Encoding as TE

import Control.Concurrent.Async (mapConcurrently)
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Simple

import Hh200.Types
import Hh200.Graph (connect, plot)
import Debug.Trace
import qualified Data.List.NonEmpty as Ls (NonEmpty(..))
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Char8 as BS
import qualified Data.CaseInsensitive as CaseInsensitive (mk)
import qualified Data.ByteString.Lazy.Char8 as L8

import qualified Hh200.Http as Http

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Maybe (fromJust, fromMaybe)
import Data.List (find)
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header (HeaderName)
import Control.Monad (foldM, forM)
import qualified Control.Monad.Trans.RWS.Strict as Tf

import qualified Data.Text as Text
import           Data.Text (Text)
import qualified Data.Aeson as Aeson (decode, encode)
import qualified Data.Aeson.Types as Aeson (Value(..))
import qualified BEL

-- Procedure "may" fail early, "reads" a shared http-client manager instance,
-- "writes" log as it runs, modifies environment "states" while doing IO.
type ProcM = MaybeT (Tf.RWST Http.Manager Log Env IO)

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

validJsonBody :: Http.Response -> Aeson.Value
validJsonBody resp =
    case Aeson.decode (Http.getBody resp) of
        Just av -> trace ("validJsonBody:\t" ++ show av) av
        Nothing -> trace "validJsonBody NULL!!!" Aeson.Null

asBS :: Aeson.Value -> BS.ByteString
asBS (Aeson.String t) = TE.encodeUtf8 t
asBS v                = BL.toStrict (Aeson.encode v)

textOrMt :: Aeson.Value -> Text
textOrMt (Aeson.String t) = t
textOrMt _ = ""

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


-- False indicates for corresponding CallItem (perhaps on user assert) to be reported.
assertsAreOk :: Env -> Http.Response -> Maybe ResponseSpec -> IO Bool
assertsAreOk env got mrs = do
    let status =     Http.getStatus got
        expectList = expectCodesOrDefault mrs

    if status `notElem` expectList then do
        -- PICKUP merge with log telling
        putStrLn $ "Status Mismatch: Got " ++ show status ++ ", Expected " ++ show expectList  -- ??: test piped std out presentation
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
runProcM :: Script -> Http.Manager -> Env -> IO Lead
runProcM script mgr env = do
    (mci, finalEnv, log) <- Tf.runRWST (runMaybeT $ courseFrom script) mgr env
    pure $ switch (mci, finalEnv, log)

    where
    switch :: (Maybe CallItem, Env, Log) -> Lead
    switch (mci, e, l) =
        trace ("final: " ++ show l) $ case mci of
            Just ci | "default" == ciName ci -> nonLead script
            _                                -> leadFrom mci (e, l) script

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
    -- ??: work out why passing Env like this is correct/incorrect
    -- [X] dict passing
    -- [ ] concurrency
    -- Build Request and echo CallItem parts.
    buildFrom :: Env -> CallItem -> IO (Http.Request, (CallItem, Maybe ResponseSpec))
    buildFrom env ci
        -- Requests without body.
        | null (payload $ ciRequestSpec ci) = do
            struct <- parseUrl

            renderedHeaders <- renderHeaders (headers $ ciRequestSpec ci)
            -- renderedHeaders <- do
            --     let (RhsDict unrenderedHeaders) = (headers $ ciRequestSpec ci)
            --     pure [("user-agent", "quinlanarcher@gmail.com")]

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
        evalCaptures resp (env, mrs) = do
            let (RhsDict bindings) = case mrs of
                    Nothing -> RhsDict HM.empty
                    Just rs -> captures rs

            let initialLog = if HM.null bindings then [] else [CapturesStart (HM.size bindings)]

            (ext, finalLog) <- foldlWithKeyM'
                (\(acc, logs) bK (bV :: [BEL.Part]) -> do
                    v <- BEL.render acc (Aeson.String "") bV
                    pure (HM.insert bK v acc, logs ++ [Captured bK])
                )
                (HM.insert "RESP_BODY" (validJsonBody resp) env, initialLog)
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
                        lift $ Tf.tell [HttpError (show e)]
                        pure ci
                      Right gotResp -> do
                        lift $ Tf.tell [HttpStatus (statusCode $ Http.getStatus gotResp)]
                        -- Captures.
                        env <- get

                        (upsertCaptures, captureLog) <- liftIO (evalCaptures gotResp (env, mrs))
                        lift $ Tf.tell captureLog

                        -- Unless null Captures:
                        modify upsertCaptures

                        res <- liftIO $ assertsAreOk env gotResp mrs
                        case res of
                            False -> do
                                lift $ Tf.tell [AssertsFailed]
                                pure ci
                            _ -> do
                                lift $ Tf.tell [AssertsPassed]
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
    bracket (Http.newManager (useTls $ config sole))
            Http.closeManager
            (\with -> runProcM sole with HM.empty)


-- -> NonLead | DebugLead | Lead
testOutsideWorld flow@(Script { callItems = _ }) = do
    bracket (Http.newManager (useTls $ config flow))
            Http.closeManager
            (\with -> runProcM flow with HM.empty)

testOutsideWorld unexpected = do
    pure $ nonLead unexpected


-- Unminuted mode: a web service that listens to sigs for stopping hh200 from making calls.
testRps :: Script -> IO ()
testRps checked = do
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
testShotgun n checked = bracket (Http.newManager (useTls $ config checked))
                                Http.closeManager
                                (\with -> do

    let msg = "Running HTTP calls with " ++ show n ++ " parallel workers…"

    leads <- trace msg $ mapConcurrentlyBounded n (replicate n (runProcM checked with HM.empty))

    putStrLn "exit code"

    -- pure $ fromMaybe (nonLead checked) (find (not . noNews) results))  -- ??
    -- plot forks results
    -- pure $ nonLead checked
    )


-- thread's:  system start-end times  script success pct  memory
-- data DataPoint = DataPoint
--   { shotgunN :: Int
--   , shotgunPct :: Double  -- Percentage of user assertions satisfying responses.
--   }

-- mkDataPoint :: Int -> DataPoint
-- mkDataPoint n = DataPoint { shotgunN = n, shotgunPct = 0.0 }
