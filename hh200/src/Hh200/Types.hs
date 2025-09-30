{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hh200.Types
  ( HostInfo(..), defaultHostInfo
  , RequestSpec(..)
  , ResponseSpec(..)
  , DepsClause(..), defaultDepsClause
  , Script(..), ScriptConfig(..), defaultScriptConfig, dbgScriptConfig
  , Snippet(..)
  , pCallItem, CallItem, firstFailing
  , UppercaseString, expectUpper
  , testOutsideWorld
  , present
  , module Network.HTTP.Types.Status
  , Binding, mkCaptures
  ) where

import Debug.Trace
import qualified Data.List.NonEmpty as Ls (NonEmpty(..))
import qualified Data.Char as Char (isUpper, toUpper)
import qualified Data.ByteString       as S8
import qualified Data.HashMap.Strict as HM
import qualified Data.HashTable.IO as H
import qualified Data.ByteString.Char8 as BS
import GHC.Generics

import Network.HTTP.Simple (setRequestMethod)
import Network.HTTP.Client.TLS
import qualified Data.ByteString.Lazy.Char8 as L8

import qualified Network.HTTP.Client as Prim
  ( newManager, parseRequest, httpLbs, method, requestBody, requestHeaders, responseStatus, responseBody
  , Manager, Response, Request, RequestBody(..), HttpException
  )
import Network.HTTP.Types.Method
import Control.Exception (try)
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Maybe (fromJust)
import Network.HTTP.Types.Status
import Control.Monad (forM_, when, unless, foldM)
import qualified Control.Monad.Trans.RWS.Strict as Tf

import System.Directory (doesFileExist)
import Control.Exception
import Control.Concurrent (ThreadId)
import qualified Control.Concurrent as Base

import qualified Data.Vector as Vec
import qualified Data.Vector as Aeson (Vector)
import qualified Data.Text as Text
import           Data.Text (Text)
import qualified Text.Parsec.Error as Aeson (ParseError)
import qualified Data.Aeson as Aeson (eitherDecode, decode)
import qualified Data.Aeson.Types as Aeson (Value(..))
import Data.Aeson.QQ.Simple (aesonQQ)
import qualified Data.Aeson.JSONPath as Aeson (jsonPath, query)
import qualified BEL (renderTemplate)

-- type Salad = (Prim.Request, (CallItem, Maybe ResponseSpec))  -- ??

headerJson = ("Content-Type", "application/json")

newtype RhsDict = RhsDict (HM.HashMap String Rhs)
    deriving (Show, Eq)

data Rhs =
    RhsQuoted Text
  | RhsJsonpath Text
  | RhsBel Text
  deriving (Show, Eq)

newtype StringString = StringString (HM.HashMap String String)
    deriving (Show, Eq)

mkCaptures :: [Binding] -> RhsDict
mkCaptures bindings = RhsDict (HM.fromList $ map fromParsed bindings)
    where
    fromParsed :: Binding -> (String, Rhs)
    fromParsed (a, b) = (a, RhsBel (Text.pack b))

mock :: Aeson.Value = [aesonQQ| { "data": { "token": "abcde9" } } |]

-- Expect one matching Value or ??log.
queryBody :: String -> Aeson.Value -> Maybe Aeson.Value
queryBody q root =
    case Aeson.query q root of
        Left _ -> Nothing
        Right v -> case Vec.uncons v of
            Nothing -> Nothing
            Just (one, _) -> Just one

newtype Snippet = Snippet L8.ByteString

data DepsClause = DepsClause
  { deps :: [String]
  , itemName :: String
  }
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
  , headers :: [Binding]
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
  , asserts :: [String]  -- List of untyped expr line, input for safeBel.
  }
  deriving (Show, Eq)

data CheckingError
  = CheckingError Text
  deriving (Show, Eq)

type CheckingResult a = Either CheckingError a

-- Early Left indicates for corresponding CallItem (perhaps with user assert)
-- to be reported.
assertsEval :: Env -> Prim.Response a -> Maybe ResponseSpec -> CheckingResult ()
assertsEval env got mrs = do
    -- First off, status code assertion.
    case elem (Prim.responseStatus got) expectCodes of

        -- Skip evaluating user assertion lines on unexpected status code.
        False -> Left (CheckingError "unexpected status code")

        -- Even though it practically can short-circuit, we're letting it fold
        -- all the input lines to print BEL logs at once.
        True ->
            Right ()
            -- ??
            -- fold clo True linesOrMt

            -- h linesOrMt

            -- case BEL.renderTemplate env (head linesOrMt) of
            --     -- Rights: Evaluated all user assertion lines.
            --     --
            --     Right "false" ->
            --         -- Some assertion point terminates as false value.
            --         Right ()
            --     Right "true" ->
            --         -- All good: assertion points terminate as true values.
            --         Right ()
            --
            --     -- Lefts: Evaluation interrupted.
            --     --
            --     _ -> -- Right _ | Left _
            --         -- Logs printed; unexpected end result.
            --         Left (CheckingError "upstream error")
    where
    clo :: String -> Either String String
    clo = BEL.renderTemplate env
    h :: [String] -> CheckingResult ()
    h lines =
        Right ()
    linesOrMt = case mrs of
        Just rs -> asserts rs
        _ -> []
    expectCodes = expectCodesOrDefault mrs

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
  , subjects1 = [Subject "a"]
  , subjects = (Subject "a") Ls.:| []
  }

cfgs :: ScriptConfig -> [(String, Maybe String)]
cfgs ScriptConfig {..} =
    [ ("retries", Just $ show retries)
    , ("maxDuration", Just "maxDuration |> show")
    ]

class PrettyPrint a where
    pp :: a -> String

instance PrettyPrint ScriptConfig where
    --        as concrete script header  #! retries 1
    --                                   #! max-duration 1m
    -- cfg in cfgs, map "#! {cfg.0} {cfg.1}"
    pp x = show x

newtype UppercaseString = UppercaseString String
    deriving (Show, Eq)
-- ??: annotate partial functions, they're encouraged for static phase
expectUpper :: String -> UppercaseString
expectUpper s | all (`elem` ['A'..'Z']) s = UppercaseString s

asMethod :: UppercaseString -> BS.ByteString
asMethod (UppercaseString s) = BS.pack s

-- ??: static phase
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
    , headers = [], payload = "", opts = []
    }
  , ciResponseSpec = Nothing
  }
callItemIsDefault :: CallItem -> Bool
callItemIsDefault CallItem { ciName } = ciName == "default"

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

-- gatherHostInfo :: IO HostInfo

debugLead :: Lead
debugLead = DebugLead
  { firstFailing = Nothing
  , hostInfo = defaultHostInfo
  , echoScript = Nothing
  }

data InternalError = OutOfBounds
                   | Todo
    deriving (Show, Eq)

data HhError = LibError
             | SystemError
             | PointableError
    deriving (Show)

type Binding = (String, String)

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
    pure (switch results)

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

-- trimQuotes :: String -> String
-- trimQuotes s =
--   case s of
--     ('"':xs) -> case reverse xs of
--                   ('"':ys) -> reverse ys
--                   _        -> s
--     _        -> s

safeBel :: Env -> String -> String
safeBel env s =
    -- ??: trimQuotes
    case BEL.renderTemplate env s of
        Left og -> og
        Right res -> res

-- Course is procedure in a stack form that will return the CallItem
-- that turned out to be failing.
courseFrom :: Script -> ProcM CallItem
courseFrom x = do
    env <- get
    pairs <- liftIO (mapM (buildFrom env) (callItems x))
    -- pairs <- liftIO (mapM buildFrom (callItems x))
    liftIOWithMgr pairs

    where
    -- ??: work out why passing Env like this is correct/incorrect
    -- [X] dict passing
    -- Build Request and echo CallItem parts.
    buildFrom :: Env -> CallItem -> IO (Prim.Request, (CallItem, Maybe ResponseSpec))
    buildFrom env ci
        -- Requests without body.
        | null (payload $ ciRequestSpec ci) = do
            let fmt :: String = safeBel env (url $ ciRequestSpec ci)

            struct :: Prim.Request <- Prim.parseRequest fmt
            dorp (ci, (ciResponseSpec ci)) struct
              { Prim.method = asMethod (verb $ ciRequestSpec ci)
              }

        -- Requests with json body.
        | otherwise = do
            let bel = safeBel env (payload $ ciRequestSpec ci)
            struct <- Prim.parseRequest (url $ ciRequestSpec ci)
            dorp (ci, (ciResponseSpec ci)) struct
              { Prim.method = asMethod (verb $ ciRequestSpec ci)
              , Prim.requestHeaders = [headerJson]
              , Prim.requestBody = rawPayload (trace (show bel) bel)
              }
    -- buildFrom :: CallItem -> IO (Prim.Request, (CallItem, Maybe ResponseSpec))
    -- buildFrom ci
    --     -- Requests without body.
    --     | null (payload $ ciRequestSpec ci) = do
    --         env <- get
    --         let fmt :: String = safeBel env (url $ ciRequestSpec ci)
    --
    --         struct :: Prim.Request <- Prim.parseRequest fmt
    --         dorp (ci, (ciResponseSpec ci)) struct
    --           { Prim.method = asMethod (verb $ ciRequestSpec ci)
    --           }
    --
    --     -- Requests with json body.
    --     | otherwise = do
    --         env <- get
    --         let bel = safeBel env (payload $ ciRequestSpec ci)
    --         struct <- Prim.parseRequest (url $ ciRequestSpec ci)
    --         dorp (ci, (ciResponseSpec ci)) struct
    --           { Prim.method = asMethod (verb $ ciRequestSpec ci)
    --           , Prim.requestHeaders = [headerJson]
    --           , Prim.requestBody = rawPayload (trace (show bel) bel)
    --           }

        where
        -- Return swapped product (`dorp` is prod reversed).
        dorp :: a -> b -> IO (b, a)
        dorp a b = pure (b, a)

        rawPayload :: String -> Prim.RequestBody
        rawPayload x = Prim.RequestBodyLBS $ L8.pack x

    -- Results arrive here!
    liftIOWithMgr :: [(Prim.Request, (CallItem, Maybe ResponseSpec))] -> ProcM CallItem
    liftIOWithMgr pairs = do
        mgr :: Prim.Manager <- ask
        h mgr pairs

        where
        validJsonBody :: Prim.Response L8.ByteString -> Aeson.Value
        validJsonBody resp =
            let avOpt :: Maybe Aeson.Value = Aeson.decode (Prim.responseBody resp) in
            case avOpt of
                Nothing -> Aeson.Null
                Just av -> av

        -- Reduce captures to Env extensions.
        evalCaptures :: Prim.Response L8.ByteString
                     -> Maybe ResponseSpec
                     -> Env -> Env
        evalCaptures resp mrs e =
            let (RhsDict bindings) = case mrs of
                    Nothing -> RhsDict HM.empty
                    Just rs -> captures rs

            in HM.foldlWithKey'
                (\(acc :: Env) bK (bV :: Rhs) ->
                    (case bV of
                        -- _ -> HM.insert "bK" (Aeson.String "rhs"))
                        RhsQuoted rhs -> HM.insert bK (Aeson.String rhs)
                        RhsJsonpath rhs ->
                            case queryBody (show rhs) (validJsonBody resp) of
                                Just val -> HM.insert bK val
                                Nothing -> (\x -> x)
                        RhsBel rhs ->
                            HM.insert bK $ Aeson.String (Text.pack (safeBel acc (show rhs))))
                    acc)
                e bindings

        -- response = {captures, asserts}. request = {configs ("options" in hurl), cookies}
        h :: Prim.Manager -> [(Prim.Request, (CallItem, Maybe ResponseSpec))] -> ProcM CallItem
        h mgr pairs = case pairs of
                [] -> emptyHanded

                (req, (ci, mrs)) : rest -> do
                    got :: Prim.Response L8.ByteString <- liftIO (Prim.httpLbs req mgr)
                    let kk = Prim.responseBody got

                    tell [L8.unpack kk]
                    liftIO $ putStrLn "h rec:"
                    -- Captures.

                    env <- get
                    let ec = evalCaptures got mrs
                    let ne :: Env = ec env
                    -- PICKUP bel " today()"
                    liftIO $ putStrLn ("bel evaled:" ++ show (HM.lookupDefault (Aeson.Number 9) "START" ne))

                    let nnn = 911
                    liftIO $ putStrLn (show nnn)

                    -- Unless null Captures:
                    modify $ evalCaptures got mrs -- modify accepts `Env -> Env` transformation.

                    -- Asserts.
                    -- nice to have bel lines statically checked regardless of the outside
                    -- world but full evaluation of bel requires outside world results.
                    --
                    -- a line doesn't mutate the Env.
                    --
                    -- world's and spec's status matching



                    case assertsEval env got mrs of
                        Left (CheckingError _msg) -> pure ci
                        Right () -> h mgr rest


expectCodesOrDefault :: Maybe ResponseSpec -> [Status]
expectCodesOrDefault x =
    case x of
        Nothing -> [status200]
        Just s -> statuses s

testOutsideWorld :: Script -> IO Lead

-- -> NonLead
testOutsideWorld static@(Script { config = _, callItems = [] }) = do
    pure $ nonLead static

-- -> NonLead | DebugLead | Lead
testOutsideWorld sole@(
    Script
      { config = ScriptConfig { subjects }
      , callItems = [_] }) = do
    mgr <- Prim.newManager tlsManagerSettings
    let envNew :: Env = HM.fromList
            [ ("yyyymmdd", Aeson.String "19700101")
            , ("undefined", Aeson.String "falsy")
            ]
    runProcM sole mgr envNew

-- -> NonLead | DebugLead | Lead
testOutsideWorld flow@(Script { callItems }) = do
    mgr <- Prim.newManager tlsManagerSettings
    runProcM flow mgr HM.empty


present :: Lead -> Maybe String
present (NonLead {}) = Nothing
present x = Just $ show x
