{-# LANGUAGE BangPatterns #-} 
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hh200.Types
  ( HostInfo(..), defaultHostInfo
  , RequestSpec(..)
  , ResponseSpec(..)
  , DepsClause(..), defaultDepsClause
  , Script(..), ScriptConfig(..), defaultScriptConfig, dbgScriptConfig
  , Snippet(..)
  , pCallItem, CallItem(..), firstFailing, callItemIsDefault
  , UppercaseString, expectUpper, showVerb
  , testOutsideWorld
  , module Network.HTTP.Types.Status
  , Binding
  , RhsDict(..)
  , show'
  , present, noNews
  , oftenBodyless
  , assertsAreOk
  ) where

import Debug.Trace
import qualified Data.List.NonEmpty as Ls (NonEmpty(..))
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Char8 as BS

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
import Control.Monad (foldM)
import qualified Control.Monad.Trans.RWS.Strict as Tf


import qualified Data.Text as Text
import           Data.Text (Text)
import qualified Data.Aeson as Aeson (decode)
import qualified Data.Aeson.Types as Aeson (Value(..))
import qualified BEL


validJsonBody :: Prim.Response L8.ByteString -> Aeson.Value
validJsonBody resp =
    case Aeson.decode (Prim.responseBody resp) of
        Just av -> trace ("validJsonBody:\t" ++ show av) av
        Nothing -> trace "validJsonBody NULL!!!" Aeson.Null


foldlWithKeyM' :: (Monad m) => (a -> k -> v -> m a)
                            -> a
                            -> HM.HashMap k v
                            -> m a
foldlWithKeyM' f z0 hm = foldM step z0 (HM.toList hm)
    where
    step !acc (k, v) = f acc k v

show' :: Text -> String
show' t = trimQuotes $ show t

trimQuotes :: String -> String
trimQuotes s =
  case s of
    ('"':xs) -> case reverse xs of
                  ('"':ys) -> reverse ys
                  _        -> s
    _        -> s


-- type Salad = (Prim.Request, (CallItem, Maybe ResponseSpec))  -- ??

headerJson :: (HeaderName, BS.ByteString)
headerJson = ("Content-Type", "application/json")

newtype RhsDict = RhsDict (HM.HashMap String BEL.Part)
    deriving (Show, Eq)

newtype Snippet = Snippet L8.ByteString

data DepsClause = DepsClause
  { deps :: [String]
  , itemName :: String
  }
defaultDepsClause :: DepsClause
defaultDepsClause = DepsClause { deps = [], itemName = "" }

pCallItem :: DepsClause -> RequestSpec -> Maybe ResponseSpec -> CallItem
pCallItem dc rs opt =
    CallItem
      { ciDeps = deps dc
      , ciName = itemName dc
      , ciRequestSpec = rs
      , ciResponseSpec = opt
      }


data Script =
    Script
      { config :: ScriptConfig
      , callItems :: [CallItem]
      }
  | StaticScript
      { config :: ScriptConfig
      , callItems :: [CallItem]
      }
  | SoleScript
      { config :: ScriptConfig
      , callItems :: [CallItem]
      }
  deriving (Show, Eq)

data RequestSpec = RequestSpec
  { verb :: UppercaseString
  , url :: String
  , headers :: RhsDict
  , payload :: String
  , opts :: [String]
  -- , configs :: RhsDict
  -- , cookies :: RhsDict
  }
  deriving (Show, Eq)

data ResponseSpec = ResponseSpec
  { statuses :: [Status]
  , output :: [String]
  , captures :: RhsDict
  , asserts :: [String]  -- List of untyped expr line, input for evaluator.
  -- , asserts :: [Text]  -- List of untyped expr line, input for evaluator.
  }
  deriving (Show, Eq)

        -- Skip evaluating anything else (including user assertion lines) on unexpected
        -- status code.
        -- Even though it's more fitting to the fail-fast philosophy by short-circuiting,
        -- we're mapping all the input lines to print BEL logs at once.

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

type Duration = Int
newtype Subject = Subject String
    deriving (Show, Eq)

data ScriptConfig = ScriptConfig
  { retries :: Int
  , maxDuration :: Maybe Duration
  , subjects1 :: [Subject]
  , subjects :: Ls.NonEmpty Subject
  } deriving (Show, Eq)

dbgScriptConfig :: ScriptConfig
dbgScriptConfig = ScriptConfig
  { retries = 0
  , maxDuration = Nothing
  , subjects1 = [Subject "custommm"]
  , subjects = (Subject "a") Ls.:| []
  }

defaultScriptConfig :: ScriptConfig
defaultScriptConfig = ScriptConfig
  { retries = 0
  , maxDuration = Nothing
  , subjects1 = [Subject "default a"]
  , subjects = (Subject "a") Ls.:| []
  }

newtype UppercaseString = UppercaseString String
    deriving (Show, Eq)

showVerb :: UppercaseString -> String
showVerb (UppercaseString s) = s

-- | __Partial__: Asserts uppercase input.
expectUpper :: String -> UppercaseString
expectUpper s | all (`elem` ['A'..'Z']) s = UppercaseString s
expectUpper _ = undefined

asMethod :: UppercaseString -> BS.ByteString
asMethod (UppercaseString s) = BS.pack s

oftenBodyless :: UppercaseString -> Bool
oftenBodyless (UppercaseString s) = elem s ["GET", "HEAD", "OPTIONS", "TRACE"]

data CallItem = CallItem
  { ciDeps :: [String]
  , ciName :: String
  , ciRequestSpec :: RequestSpec
  , ciResponseSpec :: Maybe ResponseSpec
  } deriving (Show, Eq)

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
callItemIsDefault :: CallItem -> Bool
callItemIsDefault CallItem { ciName } = ciName == "default"

-- gatherHostInfo :: IO HostInfo

-- Host computer info: /etc/resolv.conf, execution time,
data HostInfo = HostInfo
  { hiUptime ::    Maybe String
  , hiHh200Conf :: Maybe ScriptConfig
  } deriving (Show, Eq)

defaultHostInfo :: HostInfo
defaultHostInfo = HostInfo
  { hiUptime = Nothing
  , hiHh200Conf = Nothing
  }

-- Everything one could ask for when debugging a failing script.
data Lead =
    Lead
      { firstFailing :: Maybe CallItem
      , hostInfo ::     HostInfo
      , interpreterInfo :: (Env, Log)
      , echoScript ::   Maybe Script
      }
  | DebugLead
      { firstFailing :: Maybe CallItem
      , hostInfo ::     HostInfo
      , interpreterInfo :: (Env, Log)
      , echoScript ::   Maybe Script
      }
  | NonLead
      { firstFailing :: Maybe CallItem
      , hostInfo ::     HostInfo
      , interpreterInfo :: (Env, Log)
      , echoScript ::   Maybe Script
      }
  deriving (Show)

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

data InternalError = OutOfBounds
                   | Todo
    deriving (Show, Eq)

data HhError = LibError
             | SystemError
             | PointableError
    deriving (Show)

type Binding = (String, BEL.Part)

--------------------------------
-- EXECUTIVE SUMMARY OF HH200 --
--------------------------------
-- terminating:  analyze  testOutsideWorld  present
-- steps output (1. linter hints; 2. reality; 3. counter-example)
--
-- early 1:
-- lexer/parser, http idioms (GET with payload, webdav status code misuse)
--
-- early 2:
-- status codes mismatch, duration, filesystem, thread cancelled, offline


type Env = HM.HashMap String Aeson.Value
type Log = [String]

-- Procedure "may" fail early, "reads" a shared http-client manager instance,
-- "writes" log as it runs, modifies environment "states" while doing IO.
type ProcM = MaybeT (Tf.RWST Prim.Manager Log Env IO)

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

-- ??: after fearless across subjects done, generalize mvar to other
-- distributed-systems primitives: message queue, kubernetes

-- Env is modified, Log appended throughout the body of `courseFrom`.
--
-- A failing CallItem is not always found, we encode None/Nothing
-- value with defaultCallItem to simplify code.
--
emptyHanded :: ProcM CallItem
emptyHanded = pure defaultCallItem

textOrMt :: Aeson.Value -> Text
textOrMt (Aeson.String t) = t
textOrMt _ = ""

stringOrMt :: Aeson.Value -> String
stringOrMt v = Text.unpack $ textOrMt v

-- Course is procedure in a stack form that will return the CallItem
-- that turned out to be failing.
courseFrom :: Script -> ProcM CallItem
courseFrom x = do
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

            dorp (ci, (ciResponseSpec ci)) struct
              { Prim.method = asMethod (verb $ ciRequestSpec ci)
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
        parseUrl :: IO Prim.Request
        parseUrl = do
            rendered <- stringRender (url $ ciRequestSpec ci)
            Prim.parseRequest rendered

        -- Return swapped product (`dorp` is prod reversed).
        dorp :: a -> b -> IO (b, a)
        dorp a b = pure (b, a)

        stringRender :: String -> IO String
        stringRender s = do
            rendered :: Aeson.Value <- BEL.render env (Aeson.String "") (BEL.partitions $ Text.pack s)
            trace ("s\t" ++ s ++ ";\n" ++ "rendered\t" ++ show rendered ++ ";") (pure $ stringOrMt rendered)

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
                     -> IO (Env -> Env)

        evalCaptures resp (env, mrs) = do
            let (RhsDict bindings) = case mrs of
                    Nothing -> RhsDict HM.empty
                    Just rs -> captures rs

            ext <- (foldlWithKeyM'
                (\(acc :: Env) bK (bV :: BEL.Part) -> do
                    v <- (case bV of
                        BEL.R t -> trace ("R:" ++ show t) $ pure (Aeson.String t)
                        BEL.L e -> trace ("L:" ++ show e) $ BEL.eval acc e)

                    pure $ HM.insert bK v acc)
                -- Eagerly from the beginning of the fold, acc Env is initialized
                -- with RESP_BODY.
                (HM.insert "RESP_BODY" (validJsonBody resp) env)
                bindings)

            pure (\_env -> ext)

        -- response = {captures, asserts}. request = {configs ("options" in hurl), cookies}
        h :: Prim.Manager -> [(Prim.Request, (CallItem, Maybe ResponseSpec))] -> ProcM CallItem
        h mgr list = case list of
                [] -> emptyHanded

                (req, (ci, mrs)) : rest -> do
                    gotResp :: Prim.Response L8.ByteString <- liftIO (Prim.httpLbs req mgr)

                    -- Captures.
                    env <- get

                    upsertCaptures :: (Env -> Env) <- liftIO (evalCaptures gotResp (env, mrs))

                    -- Unless null Captures:
                    modify upsertCaptures

                    -- Asserts.
                    -- nice to have bel lines statically checked regardless of the outside
                    -- world but full evaluation of bel requires outside world results.
                    --
                    -- a line doesn't mutate the Env.
                    --
                    -- world's and spec's status matching

                    res <- liftIO $ assertsAreOk env (trace ("gotResp:" ++ show (Prim.responseBody gotResp)) gotResp) mrs
                    case res of
                        False -> pure ci
                        _ -> h mgr rest


expectCodesOrDefault :: Maybe ResponseSpec -> [Status]
expectCodesOrDefault mrs =
    case mrs of
        Nothing -> [status200]
        Just rs -> case statuses rs of
            [] -> [status200]
            expectCodes -> expectCodes

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


-- present :: Lead -> Maybe String
-- present (NonLead {}) = Nothing
-- present x = Just $ show x

noNews :: Lead -> Bool
noNews (NonLead {}) = True
noNews _ = False

present :: CallItem -> String
present ci = (showVerb $ verb $ ciRequestSpec ci) ++ " " ++ (url $ ciRequestSpec ci)
 -- ++ "\n" ++ (show $ headers $ ciRequestSpec ci)  -- ?? show hashmap, if not empty
 ++ "\n" ++ (payload $ ciRequestSpec ci)
