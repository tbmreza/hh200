{-# LANGUAGE BangPatterns #-}
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
  , Binding
  , RhsDict(..)
  , show'
  ) where

import Debug.Trace
import qualified Data.List.NonEmpty as Ls (NonEmpty(..))
import qualified Data.Char as Char (isUpper, toUpper)
import qualified Data.ByteString       as S8
import qualified Data.HashMap.Strict as HM
import qualified Data.HashTable.IO as H
import qualified Data.ByteString.Char8 as BS
-- import Data.Key
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
import qualified Data.Text.IO as Text
import           Data.Text (Text)
import qualified Text.Parsec.Error as Aeson (ParseError)
import qualified Data.Aeson as Aeson (eitherDecode, decode)
import qualified Data.Aeson.Types as Aeson (Value(..))
import Data.Aeson.QQ.Simple (aesonQQ)
import qualified Data.Aeson.JSONPath as Aeson (jsonPath, query)
-- import qualified BEL (render, eval)
-- import qualified BEL (renderTemplateText)
import qualified BEL

-- tor :: Env -> Env
-- tor = \e -> e

foldlWithKeyM'
  :: (Monad m)
  => (a -> k -> v -> m a)
  -> a
  -> HM.HashMap k v
  -> m a
foldlWithKeyM' f z0 hm =
  foldM step z0 (HM.toList hm)
  where
    step !acc (k, v) = f acc k v

chk :: IO ()
chk = do
    let hm :: Env = HM.fromList
            [ ("yyyymmdd", Aeson.String "19700101")
            , ("undefined", Aeson.String "falsy")
            ]
    -- let hm = HM.fromList [("a", 1), ("b", 2), ("c", 3)]
    total <- foldlWithKeyM' (\acc k v -> do
              -- putStrLn $ "Visiting " ++ k
              -- pure (acc + v)
              pure acc
            ) 0 hm
    -- print total
    pure ()


isFalse :: Aeson.Value -> Bool
isFalse (Aeson.Bool False) = True
isFalse _ = False

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

headerJson = ("Content-Type", "application/json")

newtype RhsDict = RhsDict (HM.HashMap String BEL.Part)
    deriving (Show, Eq)

-- mtRhsDict :: RhsDict
-- mtRhsDict = RhsDict HM.empty


-- newtype RhsDict1 = RhsDict1 (HM.HashMap String Rhs)
--     deriving (Show, Eq)

-- data Rhs =
--     RhsQuoted Text
--   | RhsJsonpath Text
--   | RhsBel Text
--   deriving (Show, Eq)

-- newtype StringString = StringString (HM.HashMap String String)
--     deriving (Show, Eq)

-- mkCaptures :: [Binding] -> RhsDict1
-- mkCaptures bindings = RhsDict1 (HM.fromList $ map fromParsed bindings)
--     where
--     fromParsed :: Binding -> (String, Rhs)
--     fromParsed (a, b) = (a, RhsBel (Text.pack b))

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
  -- , url :: Text
  -- , payload :: Text
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
        False -> pure False
        _ -> do
            values :: [Aeson.Value] <- mapM (BEL.eval env) linesOrMt  -- ?? ensure desired BEL prints
            pure $ notElem (Aeson.Bool False) values
    where
    -- linesOrMt :: [String]
    linesOrMt :: [Text]
    linesOrMt = case mrs of
        Just rs -> map Text.pack $ asserts rs
        _ -> []

-- assertsEval1 :: Env -> Prim.Response a -> Maybe ResponseSpec -> CheckingResult ()
-- assertsEval1 env got mrs = do trace ("assertsEval1:" ++ show linesOrMt)
--     (case elem (Prim.responseStatus got) (expectCodesOrDefault mrs) of
--         False -> Left ()
--
--         True ->
--             let mapped = map bel linesOrMt in
--             let msg = "lines:\t" ++ show (linesOrMt)
--                     ++ "\nmapped:\t" ++ show mapped
--                     ++ "\nrender:\t" ++ show "red"
--                     in
--             trace msg (Right ()))
--     where
--     bel :: String -> String
--
--     -- clo :: Text -> Either Text Text
--     -- clo = BEL.renderTemplateText env
--     h :: [String] -> CheckingResult ()
--     h lines =
--         Right ()
--     linesOrMt :: [String]
--     linesOrMt = case mrs of
--         Just rs -> asserts rs
--         _ -> []
--     expectCodes = expectCodesOrDefault mrs

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

-- | __Partial__: Asserts uppercase input.
expectUpper :: String -> UppercaseString
expectUpper s | all (`elem` ['A'..'Z']) s = UppercaseString s

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
    let (_, finalEnv, _) = results
    putStrLn $ ("finalEnv:\n" ++ show finalEnv)
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

-- render :: Env -> Aeson.Value -> [Part] -> IO Aeson.Value
-- partitions :: Text -> [Part]

textOrMt :: Aeson.Value -> Text
textOrMt (Aeson.String t) = t
textOrMt _ = ""

stringOrMt :: Aeson.Value -> String
stringOrMt v = Text.unpack $ textOrMt v

-- Context-free evaluation.
--     where
--     e :: Env = HM.fromList [ ("yyyymmdd", Aeson.String "19700101")
--                            , ("undefined", Aeson.String "falsy")
--                            ]

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
            struct <- parseUrl env ci

            dorp (ci, (ciResponseSpec ci)) struct
              { Prim.method = asMethod (verb $ ciRequestSpec ci)
              }

        -- Requests with json body.
        | otherwise = do
            struct <- parseUrl env ci
            rb <- stringRender (payload $ ciRequestSpec ci)

            dorp (ci, (ciResponseSpec ci)) struct
              { Prim.method = asMethod (verb $ ciRequestSpec ci)
              , Prim.requestHeaders = [headerJson]
              , Prim.requestBody = rawPayload $ rb
              }

        where
        parseUrl :: Env -> CallItem -> IO Prim.Request
        parseUrl env ci = do
            rendered <- stringRender (url $ ciRequestSpec ci)
            Prim.parseRequest rendered

        -- Return swapped product (`dorp` is prod reversed).
        dorp :: a -> b -> IO (b, a)
        dorp a b = pure (b, a)

        stringRender :: String -> IO String
        -- stringRender s = pure s
        stringRender s = do
            rendered :: Aeson.Value <- BEL.render env (Aeson.String "") (BEL.partitions $ Text.pack s)
            pure $ stringOrMt rendered

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
                     -> (Env, Maybe ResponseSpec)
                     -> IO (Env -> Env)

        evalCaptures resp (env, mrs) = do
            let (RhsDict bindings) = case mrs of
                    Nothing -> RhsDict HM.empty
                    Just rs -> captures rs

            ext <- foldlWithKeyM'
                (\acc bK (bV :: BEL.Part) -> do  -- ??
                -- (\acc bK (bV :: Rhs) -> do
                    evaled :: Aeson.Value <- BEL.eval acc "rhs"
                    emaled :: Aeson.Value <- (case True of _ -> do pure Aeson.Null)
                    pure $ HM.insert bK (case bV of
                        _ -> do
                            (Aeson.String "")
                        _ -> (Aeson.String "")) acc)
                env
                bindings


            -- -- foldlWithKey' :: FoldableWithKey t => (b -> Key t -> a -> b) -> b -> t a -> b
            -- pure (\e ->
            --     HM.foldlWithKey'
            --         (\(acc :: Env) (bK :: String) (bV :: Rhs) ->
            --             let Aeson.String (retr :: Text) = HM.lookupDefault (Aeson.String "") bK e in
            --             (case bV of
            --                 RhsQuoted rhs -> HM.insert bK (Aeson.String rhs)
            --                 RhsJsonpath rhs ->
            --                     case queryBody (show rhs) (validJsonBody resp) of
            --                         Just val -> HM.insert bK val
            --                         Nothing -> (\x -> x)
            --                 RhsBel (rhs :: Text) ->
            --                     -- let evaled = Aeson.String (Text.pack (safeBel1 acc (show' rhs))) in
            --                     let evaled = Aeson.String "" in
            --                     HM.insert bK evaled)
            --             acc)
            --         (e ::        HM.HashMap String Aeson.Value)
            --         (bindings :: HM.HashMap String Rhs))

            pure (\e -> ext)







        -- evalCaptures :: Prim.Response L8.ByteString
        --              -> Maybe ResponseSpec
        --              -> Env -> Env
        -- evalCaptures resp mrs e =
        --     let (RhsDict bindings) = case mrs of
        --             Nothing -> RhsDict HM.empty
        --             Just rs -> captures rs
        --
        --     in HM.foldlWithKey'
        --         (\(acc :: Env) (bK :: String) (bV :: Rhs) ->
        --             let Aeson.String (retr :: Text) = HM.lookupDefault (Aeson.String "") bK e in
        --             (case bV of
        --                 RhsQuoted rhs -> HM.insert bK (Aeson.String rhs)
        --                 RhsJsonpath rhs ->
        --                     case queryBody (show rhs) (validJsonBody resp) of
        --                         Just val -> HM.insert bK val
        --                         Nothing -> (\x -> x)
        --                 RhsBel (rhs :: Text) ->
        --                     -- let evaled = Aeson.String (Text.pack (safeBel1 acc (show' rhs))) in
        --                     let evaled = Aeson.String "" in
        --
        --                     HM.insert bK evaled)
        --             acc)
        --         e bindings

        -- response = {captures, asserts}. request = {configs ("options" in hurl), cookies}
        h :: Prim.Manager -> [(Prim.Request, (CallItem, Maybe ResponseSpec))] -> ProcM CallItem
        h mgr pairs = case pairs of
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

                    res <- liftIO $ assertsAreOk env gotResp mrs
                    case res of
                        False -> pure ci
                        _ -> h mgr rest


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
