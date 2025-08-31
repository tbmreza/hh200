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
  , Script(..), ScriptConfig(..), defaultScriptConfig
  , Snippet(..)
  , pCallItem
  , UppercaseString, expectUpper
  , testOutsideWorld
  , present
  , module Network.HTTP.Types.Status
  , Binding , mtCaptures, mkCaptures
  ) where

import Debug.Trace
import qualified Data.List.NonEmpty as Ls (NonEmpty(..))
import qualified Data.Char as Char (isUpper, toUpper)
import qualified Data.ByteString       as S8
import qualified Data.HashMap.Strict as HM
import qualified Data.HashTable.IO as H
-- import qualified Data.ByteString as BS  -- ??: alex ByteString wrapper
import qualified Data.ByteString.Char8 as BS
-- import GHC.Generics (Generic)
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
import Control.Monad (forM_)
import qualified Control.Monad.Trans.RWS.Strict as Tf

import System.Directory (doesFileExist)
import Control.Exception
import Control.Concurrent (ThreadId)
import qualified Control.Concurrent as Base

import qualified Data.Aeson as Json (encode, Value(..))

headerJson = ("Content-Type", "application/json")

newtype JsonpathQueries = JsonpathQueries (HM.HashMap String String)
    deriving (Show, Eq)

mkCaptures :: [Binding] -> JsonpathQueries
mkCaptures bindings = JsonpathQueries (HM.fromList bindings)

mtCaptures :: JsonpathQueries
mtCaptures = JsonpathQueries HM.empty

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

newtype JsonStr = JsonStr String


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
    , headers :: [String]
    , payload :: String
    , opts :: [String]
    }
    deriving (Show, Eq)

data ResponseSpec = ResponseSpec
  { statuses :: [Status]
  , output :: [String]
  , captures :: JsonpathQueries
  }
  deriving (Show, Eq)

type Duration = Int
newtype Subject = Subject String
    deriving (Show, Eq)

data ScriptConfig = ScriptConfig
  { retries :: Int
  , maxDuration :: Maybe Duration
  , subjects1 :: [Subject]
  , subjects :: Ls.NonEmpty Subject
  } deriving (Show, Eq)
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
      , echoScript ::   Maybe Script
      }
  | DebugLead
      { firstFailing :: Maybe CallItem
      , hostInfo ::     HostInfo
      , echoScript ::   Maybe Script
      }
  | NonLead
      { firstFailing :: Maybe CallItem
      , hostInfo ::     HostInfo
      , echoScript ::   Maybe Script
      }
  deriving (Show)

nonLead :: Script -> Lead
nonLead x = NonLead
  { firstFailing = Nothing
  , hostInfo = defaultHostInfo
  , echoScript = Just x
  }

leadFrom :: Maybe CallItem -> Script -> Lead
leadFrom failed script = Lead
  { firstFailing = failed
  , hostInfo = defaultHostInfo
  , echoScript = Just script
  }

-- gatherHostInfo :: IO HostInfo

debugLead :: Lead
debugLead = DebugLead
  { firstFailing = Nothing
  , hostInfo = defaultHostInfo
  , echoScript = Nothing
  }

-- -- Presume http-client manager sharing.
-- type HttpM = ReaderT Prim.Manager IO
-- runHttpM :: HttpM a -> IO a
-- runHttpM action = do
--     manager <- Prim.newManager tlsManagerSettings
--     runReaderT action manager
-- -- HttpExceptionRequest
--
-- runCompiled :: HttpM a -> IO a
-- runCompiled action = do
--     manager <- Prim.newManager tlsManagerSettings
--     runReaderT action manager
--
-- mkRequest :: BS.ByteString -> String -> Maybe L8.ByteString -> HttpM L8.ByteString
-- mkRequest    methodStr    url       mBody                = do
--     initialRequest <- liftIO $ Prim.parseRequest url
--     let request = initialRequest
--             { Prim.method = methodStr
--             , Prim.requestBody = maybe mempty Prim.RequestBodyLBS mBody
--             , Prim.requestHeaders = case mBody of
--                 Just _  -> [("Content-Type", "application/json")]
--                 Nothing -> Prim.requestHeaders initialRequest
--             }
--     response <- liftIO $ Prim.httpLbs request manager
--     return $ Prim.responseBody response
--
-- httpGet :: String -> HttpM L8.ByteString
-- httpGet url = mkRequest "GET" url Nothing
--
-- httpPost :: String -> L8.ByteString -> HttpM L8.ByteString
-- httpPost url body = mkRequest "POST" url (Just body)


data InternalError = OutOfBounds
                   | Todo
    deriving (Show, Eq)

data HhError = LibError
             | SystemError
             | PointableError
    deriving (Show)

ok :: Maybe a
ok = Nothing

data TerribleException = TerribleException deriving (Show)
-- newtype TerribleException = TerribleException String deriving (Show)
instance Exception TerribleException

type Source = FilePath

type HttpVerb = BS.ByteString

type HashTable k v = H.BasicHashTable k v
type Headers = HashTable String String

type Vars = HM.HashMap String Integer

type Binding = (String, String)

varsDefault :: Vars
varsDefault = HM.fromList [("max_reruns", 2)]

class PolicyT a where
    policyOrDefault :: a -> a

instance PolicyT Policy where

    policyOrDefault Policy { maxReruns, maxRetriesPerCall, timeMillisPerCall } =

        Policy { maxReruns = orMR, maxRetriesPerCall = orMRPC, timeMillisPerCall = orTMPC } where

            orTMPC :: Maybe Int
            orTMPC = case
                timeMillisPerCall of
                    -- Nothing -> Just 60_000
                    Nothing -> Just 60000
                    var -> var

            orMRPC :: Maybe Int
            orMRPC = case
                maxRetriesPerCall of
                    Nothing -> Just 2
                    var -> var

            orMR :: Maybe Int
            orMR = case
                maxReruns of
                    Nothing -> Just 2
                    var -> var

data Policy = Policy {
    maxReruns :: Maybe Int,
    maxRetriesPerCall :: Maybe Int,
    timeMillisPerCall :: Maybe Int
    } deriving (Eq, Show, Generic)

-- Simple source position
data Pos = Pos 
    { line :: !Int
    , col  :: !Int 
    } deriving (Eq, Show)

-- Source span with start and end positions
data Span = Span 
    { spanStart :: !Pos
    , spanEnd   :: !Pos 
    } deriving (Eq, Show)

-- Location-annotated value
data Located a = Located 
    { location :: !Span
    , unLoc    :: a 
    } deriving (Show, Functor)

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




type Env = HM.HashMap String String
-- Procedure "may" fail early, "writes" log as it runs
-- and "reads" a shared http-client manager instance while doing IO.
type ProcM1  = MaybeT (WriterT Log (ReaderT Prim.Manager IO))
type Log = [String]

-- Procedure "may" fail early, "reads" a shared http-client manager instance,
-- "writes" log as it runs, modifies environment "states" while doing IO.
type ProcM = MaybeT (Tf.RWST Prim.Manager Log Env IO)

-- logMsg :: String -> ProcM ()
-- logMsg msg = lift $ tell [msg]

-- Return to user the CallItem which we suspect will fail again.
runProcM :: Script -> Prim.Manager -> Env -> IO Lead
runProcM script mgr env = do
    results <- Tf.runRWST (runMaybeT $ courseFrom script) mgr env
    return (switch results)

    where
    switch :: (Maybe CallItem, Env, Log) -> Lead
    switch (mci, e, l)
        | "default" == (ciName $ fromJust mci) =
            nonLead script
        | otherwise =
            leadFrom mci script

-- runProcM2 :: ProcM CallItem -> Prim.Manager -> Env -> IO (Maybe CallItem, Env, Log)
-- runProcM2 m mgr env =
--     Tf.runRWST (runMaybeT m) mgr env

-- ??: after fearless across subjects done, generalize mvar to other
-- distributed-systems primitives: message queue, kubernetes

-- runProcM1 :: ProcM CallItem -> Env -> Prim.Manager -> IO (CallItem, Env, Log)
-- runProcM1 m env mgr = do
--     res :: (Maybe CallItem, Env, Log) <- Tf.runRWST (runMaybeT m) mgr env
--     case res of
--         -- Functionally, a default CallItem makes sense in the event where the
--         -- compiler couldn't point to a failing CallItem in user program; likely that
--         -- the client host can't even get the expected result of sending a request to
--         -- localhost.
--         (Nothing, e, _log) -> do
--             -- ??: empty _log in upstream
--             return (defaultCallItem, e, [])
--
--         (Just suspect, e, log) -> do
--             -- return (suspect, e, log)
--             return (suspect, e, [])

-- Env is modified, Log appended throughout the body of `courseFrom`.
--
-- A failing CallItem is not always found, we encode None/Nothing
-- value with defaultCallItem to simplify code.
--
emptyHanded :: ProcM CallItem
emptyHanded = return defaultCallItem

courseFrom :: Script -> ProcM CallItem
courseFrom x = do
    pairs :: [(Prim.Request, Maybe ResponseSpec)] <- liftIO (destructure x)
    doWithMgr pairs

    where
    destructure :: Script -> IO [(Prim.Request, Maybe ResponseSpec)]
    destructure (Script { callItems }) = do
        let resb = mapM buildFrom callItems
        return []  -- ??

    -- Build Request and echo CallItem parts.
    buildFrom :: CallItem -> IO (Prim.Request, (CallItem, Maybe ResponseSpec))
    buildFrom ci
        -- Requests without body.
        | null (payload $ ciRequestSpec ci) = do
            struct :: Prim.Request <- Prim.parseRequest (url $ ciRequestSpec ci)
            dorp (ci, (ciResponseSpec ci)) struct
              { Prim.method = asMethod (verb $ ciRequestSpec ci)
              }

        -- Requests with json body.
        | oftenBodyless ciVerb = do
            struct <- Prim.parseRequest (url $ ciRequestSpec ci)
            dorp (ci, (ciResponseSpec ci)) struct
              { Prim.method = asMethod ciVerb
              , Prim.requestHeaders = [headerJson]
              , Prim.requestBody = rawPayload (payload $ ciRequestSpec ci)
              }

        | otherwise = do
            struct <- Prim.parseRequest (url $ ciRequestSpec ci)
            dorp (ci, (ciResponseSpec ci)) struct
              { Prim.method = asMethod ciVerb
              , Prim.requestHeaders = [headerJson]
              , Prim.requestBody = rawPayload (payload $ ciRequestSpec ci)
              }

        where
        ciVerb :: UppercaseString
        ciVerb = verb $ ciRequestSpec ci

        -- Return swapped product (`dorp` is prod reversed).
        dorp :: a -> b -> IO (b, a)
        dorp a b = return (b, a)


    rawPayload :: String -> Prim.RequestBody
    rawPayload x = Prim.RequestBodyLBS $ L8.pack x

    -- Results arrive here!
    -- PICKUP recursion
    doWithMgr :: [(Prim.Request, Maybe ResponseSpec)] -> ProcM CallItem
    doWithMgr pairs = do
        mgr :: Prim.Manager <- ask
        env <- get
        tell [""]

        -- let fn :: Prim.Request -> IO (Prim.Response L8.ByteString, Match) =
        --         primDo mgr

        -- results :: [(Prim.Response L8.ByteString, Match)] <- liftIO (mapM fn pairs)
        -- let sample = head results
        -- let actualCode :: Status = Prim.responseStatus sample

        case elem status200 (expectCodesOrDefault Nothing) of
            True -> emptyHanded
            _ -> return defaultCallItem
        where
        expectCodesOrDefault :: Maybe ResponseSpec -> [Status]
        expectCodesOrDefault x =
            case x of
                Nothing -> [status200]
                Just s -> statuses s
        primDo :: Prim.Manager -> Prim.Request -> IO (Prim.Response L8.ByteString)
        primDo mgr req = Prim.httpLbs req mgr
            -- where
            -- tried :: IO (Either Prim.HttpException (Prim.Response L8.ByteString))
            -- tried = try $ Prim.httpLbs req mgr


-- Course is procedure in a stack form that will return the
-- CallItem that turned out to be failing.

testOutsideWorld :: Script -> IO Lead

-- -> NonLead
testOutsideWorld static@(Script { config = _, callItems = [] }) = do
    return $ nonLead static

-- -> NonLead | DebugLead | Lead
testOutsideWorld sole@(
    Script
      { config = ScriptConfig { subjects }
      , callItems = [_] }) = do
    mgr <- Prim.newManager tlsManagerSettings
    let envNew :: HM.HashMap String String = HM.fromList [("yyyymmdd", "19700101")]
    putStrLn "dis arm"
    runProcM sole mgr envNew

    -- results :: (Maybe CallItem, Env, Log) <- runProcM2 (courseFrom sole) mgr envNew
    -- return $ case results of
    --     -- Functionally, a default CallItem makes sense in the event where the
    --     -- compiler couldn't point to a failing CallItem in user program; likely that
    --     -- the client host can't even get the expected result of sending a request to
    --     -- localhost.
    --     (Nothing, e, _log) -> do
    --         -- ??: empty _log in upstream
    --         -- return (defaultCallItem, e, [])
    --         nonLead sole
    --     (suspect, e, log) -> do
    --         -- ??: sanity check here suspect isn't defaultCallItem
    --         -- return (suspect, e, [])
    --         leadFrom suspect sole

    -- results1 :: (CallItem, Env, Log) <- runProcM1 (courseFrom sole) envNew mgr
    -- return $ case results1 of
    --     (_, _, []) -> nonLead sole
    --     (suspect, _, _) -> leadFrom (Just suspect) sole

-- -> NonLead | DebugLead | Lead
testOutsideWorld script@(Script { callItems }) = do
    -- ??: not clear if sole and script branches need to be distinct, maybe
    -- we can use this information to fully evaluate variables in compile time
    mgr <- Prim.newManager tlsManagerSettings
    let envNew :: HM.HashMap String String = HM.fromList [("yyyymmdd", "19700101")]

    runProcM script mgr envNew

--     raceToFirstFailing :: Script -> IO (Maybe CallItem)
--     raceToFirstFailing Script { config = ScriptConfig { subjects }, call_items } = do
--         hole :: Base.MVar CallItem <- Base.newEmptyMVar
--
--         let actions :: [IO (Maybe CallItem, Log)] = map (mkAction call_items) subjects
--         putStrLn (show $ length actions)
--         ids :: [ThreadId] <- forM actions $
--             -- Each action returns a thread ID.
--             \(io :: IO (Maybe CallItem, Log)) -> Base.forkIO $ do
--                 putStrLn "lam...."
--                 (res, logs) <- io
--                 putStrLn $ "show log: " ++ show logs
--
--                 case res of
--                     Just failing -> Base.putMVar hole failing
--                     Nothing -> return ()
--
--         Base.threadDelay 1000000  -- ??: main thread exits before all children. which other mvar method?
--
--         forM_ ids $ \x -> Base.killThread x
--         Base.tryReadMVar hole




present :: Lead -> String
present x = show x
    -- act io = Base.forkIO $ do
    --     failing <- io
    --     Base.putMVar hole failing

    -- -- Instruct a thread of the IO procedure it will perform.
    -- -- To be caught in race for-loop.
    -- -- consign :: Base.MVar CallItem -> String -> IO ()
    -- consign failingCallItem flow ratInfo = do
    -- -- consign failingCallItem ratInfo = do
    --     Base.putMVar failingCallItem localhost9999

