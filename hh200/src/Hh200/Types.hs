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
  ) where

import Debug.Trace
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Char as Char (isUpper, toUpper)
import qualified Data.ByteString       as S8
import qualified Data.HashMap.Strict as HM
import qualified Data.HashTable.IO as H
-- import qualified Data.ByteString as BS  -- ??: alex ByteString wrapper
import qualified Data.ByteString.Char8 as BS
import GHC.Generics (Generic)

import Network.HTTP.Simple (setRequestMethod)
import Network.HTTP.Client.TLS
import qualified Data.ByteString.Lazy.Char8 as L8

import Network.HTTP.Client
import Network.HTTP.Types.Method
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Trans.Maybe
import Network.HTTP.Types.Status
import Control.Monad (forM_)

import System.Directory (doesFileExist)
import Control.Exception
import Control.Concurrent (ThreadId)
import qualified Control.Concurrent as Base

import Data.Aeson (encode)

headerJson = ("Content-Type", "application/json")


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
  -- deriving (Show, Eq)
  deriving (Show)

-- ??: v1 of Scanner will use String before jumping to ByteString
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
  }
  deriving (Show, Eq)

type Duration = Int
newtype Subject = Subject String
    deriving (Show, Eq)

data ScriptConfig = ScriptConfig
  { retries :: Int
  , maxDuration :: Maybe Duration
  , subjects :: [Subject]
  } deriving (Show, Eq)
defaultScriptConfig :: ScriptConfig
defaultScriptConfig = ScriptConfig { retries = 0, maxDuration = Nothing, subjects = [Subject "a"] }

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

data CallItem = CallItem
  { ciDeps :: [String]
  , ciName :: String
  , ciRequestSpec :: RequestSpec
  , ciResponseSpec :: Maybe ResponseSpec
  -- } deriving (Show, Eq)
  } deriving (Show)

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

localhost9999 :: CallItem
localhost9999 = CallItem
  { ciDeps = []
  , ciName = "debug"
  , ciRequestSpec = RequestSpec { verb = expectUpper "GET", url = "http://localhost:9999/hh", headers = [], payload = "", opts = []}
  , ciResponseSpec = Nothing
  }

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
    -- ?? embed Log
    -- Lead
    --   { firstFailing :: Maybe CallItem
    --   , hostInfo ::     HostInfo
    --   , echoScript ::   Maybe Script
    --   , trace :: Log
    --   }
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
  -- deriving (Show, Eq)
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

-- Presume http-client manager sharing.
type HttpM = ReaderT Manager IO
runHttpM :: HttpM a -> IO a
runHttpM action = do
    manager <- newManager tlsManagerSettings
    runReaderT action manager
-- HttpExceptionRequest

runCompiled :: HttpM a -> IO a
runCompiled action = do
    manager <- newManager tlsManagerSettings
    runReaderT action manager

mkRequest :: BS.ByteString -> String -> Maybe L8.ByteString -> HttpM L8.ByteString
mkRequest    methodStr    url       mBody                = do
    manager <- ask
    initialRequest <- liftIO $ parseRequest url
    let request = initialRequest
            -- { method = method methodStr
            { method = methodStr
            -- { method = "GET"
            , requestBody = maybe mempty RequestBodyLBS mBody
            , requestHeaders = case mBody of
                Just _  -> [("Content-Type", "application/json")]
                Nothing -> requestHeaders initialRequest
            }
    response <- liftIO $ httpLbs request manager
    return $ responseBody response

httpGet :: String -> HttpM L8.ByteString
httpGet url = mkRequest "GET" url Nothing

httpPost :: String -> L8.ByteString -> HttpM L8.ByteString
httpPost url body = mkRequest "POST" url (Just body)


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

type HttpMethod = S8.ByteString  -- "GET" "POST"

type Source = FilePath

type HttpVerb = BS.ByteString

type HashTable k v = H.BasicHashTable k v
type Headers = HashTable String String

type Vars = HM.HashMap String Integer

-- varsEmpty = HM.empty
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

-- -- instance FromValue Policy where fromValue = genericFromTable
-- instance ToTable Policy where toTable = genericToTable
-- instance ToValue Policy where toValue = defaultTableToValue
--
-- policyDefault :: Policy
-- -- policyDefault = Policy { maxReruns = 2, maxRetriesPerCall = 2, timeMillisPerCall = 60_000 }
-- policyDefault = Policy { maxReruns = Just 2, maxRetriesPerCall = Just 2, timeMillisPerCall = Just 60000 }
--
-- instance FromValue Policy where
--     fromValue = parseTableFromValue (Policy
--         <$> optKey "max_reruns"
--         <*> optKey "max_retries_per_call"
--         <*> optKey "time_millis_per_call"
--         )

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
--
-- only legit Lead representable

-- Procedure "may" fail early, "writes" log as it runs
-- and "reads" a shared http-client manager instance while doing IO.
type ProcM = MaybeT (WriterT Log (ReaderT Manager IO))
type Log = [String]

logMsg :: String -> ProcM ()
logMsg msg = lift $ tell [msg]

-- Return to user the CallItem which we suspect will fail again.
runProcM :: ProcM CallItem -> IO (CallItem, Log)
runProcM action = do
    mgr <- newManager tlsManagerSettings
    res :: (Maybe CallItem, Log) <- runReaderT (runWriterT (runMaybeT action)) mgr
    return $ case res of
        (Nothing, log) -> (defaultCallItem, log)
        (Just suspect, log) -> (suspect, log)

-- runProcM1 :: ProcM CallItem -> IO (Maybe CallItem, Log)
-- runProcM1 action = do
--     manager <- newManager tlsManagerSettings
--     runReaderT (runWriterT (runMaybeT action)) manager


-- -- runRaceM :: ProcM a -> IO (Maybe a, Log)
-- runRaceM :: ProcM CallItem -> IO (Maybe CallItem, Log)
-- runRaceM action = do
--     manager <- newManager tlsManagerSettings
--     runReaderT (runWriterT (runMaybeT action)) manager


-- shuntGet1 :: String ->                  ProcM L8.ByteString
-- shuntGet1 = shuntHttpRequest (\req -> req { method = "GET" })
--
-- shuntPost :: L8.ByteString -> String -> ProcM L8.ByteString
-- shuntPost body = shuntHttpRequest (\req -> req
--   { method = "POST"
--   , requestBody = RequestBodyLBS body
--   , requestHeaders = ("Content-Type", "application/json") : requestHeaders req
--   })
--
-- shuntGet :: String -> ProcM L8.ByteString
-- shuntGet url = do
--   logMsg $ "Requesting: " ++ url
--   manager <- lift . lift $ ask
--   result <- liftIO $ try $ do
--     req <- parseRequest url
--     httpLbs req manager
--   case result of
--     Right resp -> do
--       let status = statusCode $ responseStatus resp
--       logMsg $ "Received status: " ++ show status
--       if status >= 200 && status < 300
--         then return $ responseBody resp
--         else do
--           logMsg $ "Non-success status: " ++ show status
--           MaybeT $ return Nothing
--     Left (HttpExceptionRequest _ content) -> do
--       logMsg $ "HttpExceptionRequest: " ++ show content
--       MaybeT $ return Nothing
--     Left err -> do
--       logMsg $ "Other exception: " ++ displayException err
--       MaybeT $ return Nothing
--
-- shuntHttpRequest :: (Request -> Request) -> String -> ProcM L8.ByteString
-- shuntHttpRequest modifyReq url = do
--   manager <- lift . lift $ ask
--   result <- liftIO $ try $ do
--     initReq <- parseRequest url
--     let req = modifyReq initReq
--     response <- httpLbs req manager
--     return $ responseBody response
--   case result of
--     Left err -> do
--       tell ["HTTP error: " ++ show (err :: HttpException)]
--       MaybeT $ return Nothing
--     Right body -> return body


courseFrom :: Script -> ProcM CallItem
courseFrom x = do
    reqs :: [Request] <- liftIO (asRequests x)
    res :: () <- shunt reqs

    -- let [ci] = callItems x
    -- let expectCodes :: [Status] = case ciResponseSpec ci of
    --         Nothing -> [status200]
    --         Just spec -> statuses spec

    return $ case True of
        _ -> defaultCallItem

    where
    asRequests :: Script -> IO [Request]
    asRequests (Script { callItems }) = do
        let res :: IO [Request] = mapM buildFrom callItems
        res

    buildFrom :: CallItem -> IO Request
    buildFrom ci
        -- Requests without body.
        | null (payload $ ciRequestSpec ci) = do
            struct :: Request <- parseRequest (url $ ciRequestSpec ci)
            return $ struct
              { method = asMethod (verb $ ciRequestSpec ci)
              }

        -- Requests with json body.
        | otherwise = do
            let body = RequestBodyLBS $ encode (payload $ ciRequestSpec ci)

            struct :: Request <- parseRequest (url $ ciRequestSpec ci)
            return $ struct
              { method = asMethod (verb $ ciRequestSpec ci)  -- ??: not in [GET HEAD OPTIONS TRACE]
              -- , requestHeaders = [("Content-Type", "application/json")]
              , requestHeaders = [headerJson]
              , requestBody = body
              }

    -- ??: try HttpException
    fire :: Manager -> Request -> IO (Response L8.ByteString)
    fire mgr req = httpLbs req mgr

    shunt :: [Request] -> ProcM ()
    shunt reqs = do
        mgr :: Manager <- ask
        let withMgr :: Request -> IO (Response L8.ByteString) = fire mgr
        let results :: IO [Response L8.ByteString] = mapM withMgr reqs

        _ <- liftIO results

        return ()

-- -- Course is procedure in a stack form that will return the
-- -- CallItem that turned out to be failing.
-- courseFrom :: Script -> ProcM CallItem
-- courseFrom x = do
--     liftIO (putStrLn $ show x)
--     -- -- build :: Request <- parseRequest (urlFrom x)
--     -- build :: Request <- parseRequest (url $ ciRequestSpec defaultCallItem)
--     -- let struct :: Request = build
--     --       { method = verb $ ciRequestSpec defaultCallItem
--     --       }
--
--     req <- liftIO (oneRequest x)
--     res :: Response L8.ByteString <- oneResponse req
--     -- liftIO $ putStrLn (show res)
--
--     let [ci] = callItems x
--     let expectCodes :: [Status] = case ciResponseSpec ci of
--             Nothing -> [status200]
--             Just spec -> statuses spec
--
--     return $ case True of
--         False -> ci
--         _ -> defaultCallItem
--
--     where
--     oneRequest :: Script -> IO Request
--     oneRequest (Script { callItems = [ci]}) = do
--         putStrLn "\tci:"
--         putStrLn $ show ci
--         let scriptUrl = url $ ciRequestSpec ci
--         build :: Request <- parseRequest scriptUrl
--         let struct :: Request = build
--               { method = asMethod (verb $ ciRequestSpec ci)
--               }
--         -- putStrLn "\tstruct:"
--         -- putStrLn $ show struct
--         return struct
--
--     oneResponse :: Request -> ProcM (Response L8.ByteString)
--     oneResponse req = do
--         liftIO $ putStrLn "oneResponse............."
--         mgr :: Manager <- ask
--         result <- liftIO $ try (httpLbs req mgr)
--         case result of
--             Left err -> do
--                 liftIO $ putStrLn "left....."
--                 tell ["HTTP error: " ++ show (err :: HttpException)]
--                 MaybeT $ return Nothing
--             Right body -> do
--                 liftIO $ putStrLn "right....."
--                 return body

testOutsideWorld :: Script -> IO Lead

-- -> NonLead
testOutsideWorld static@(Script { config = _, callItems = [] }) = do
    return $ nonLead static

-- -> NonLead | DebugLead | Lead
testOutsideWorld sole@(
    Script
      { config = ScriptConfig { subjects }
      , callItems = [soleCi] }) = do

    let ss :: [Int] = [200, 404]
    -- let jj :: [Status] = map statusFrom ss
    let nnn = Status 404 (statusMessage (mkStatus 404 ""))
    putStrLn $ "yea" ++ show nnn
    let zzz :: Status = mkStatus 200 ""

    let course :: ProcM CallItem = courseFrom sole

    -- suspect :: (CallItem, Log) <- runProcM course

    -- NonLead means status codes do match.
    -- Single: result in expectCodes
    -- Multi:  putMVar $ result in expectCodes
    case length subjects of
        1 -> do
            (suspect, crumbs) :: (CallItem, Log) <- runProcM course
            let _ = singleModeIsOk  -- ??

            return $ case null crumbs of
                False -> do
                    leadFrom (Just suspect) sole
                _ -> do
                    (nonLead sole)

        _ -> do
            -- ??: subjects is better typed as NEL
            -- let nonEmptyList = 1 :| [2, 3]
            let _ = multiModeIsOk
            return $ trace "this nonLead" (nonLead sole)

    

    where
    singleModeIsOk :: Bool  -- in which case return NonLead
    singleModeIsOk = True

    multiModeIsOk :: Bool  -- in which case return NonLead
    multiModeIsOk = True

-- -> NonLead | DebugLead | Lead
testOutsideWorld script@(Script { callItems }) = do
    let course :: ProcM CallItem = courseFrom script
    _suspect :: (CallItem, Log) <- runProcM course

    return $ nonLead script

-- testOutsideWorld1 :: Script -> MaybeT IO Lead
-- testOutsideWorld1 single@(Script { config = ScriptConfig { subjects }, call_items = [ci] }) = do
--     coming :: Maybe CallItem <- liftIO $ raceToFirstFailing single
--
--     MaybeT (return $ Just (leadFrom coming single))
--
--     where
--     mkAction :: [CallItem] -> Subject -> IO (Maybe CallItem, Log)
--     mkAction [ci] subject = do
--         putStrLn $ show subject
--
--         -- Course is procedure in a stack form that will return the
--         -- CallItem that turned out to be failing.
--         let course :: ProcM CallItem = do
--                 logMsg "building course...."
--
--                 build :: Request <- parseRequest (url $ ciRequestSpec ci)
--                 let struct = build
--                       -- { method = verb $ ciRequestSpec ci
--                       { method = verb $ ciRequestSpec ci
--                       }
--
--                 ret <- shuntHttpRequestFull struct
--
--                 let expectCodes = case ciResponseSpec ci of
--                         Nothing -> [status200]
--                         Just rc -> statuses rc
--
--
--                 logMsg "endA course...."
--                 -- case elem (responseStatus ret) expectCodes of
--                 case True of
--                     True ->
--                         -- Functionally a default CallItem makes sense in the event where the
--                         -- compiler couldn't point to a failing CallItem in user program; maybe
--                         -- the client host can't even get the expected result of sending a request to
--                         -- localhost.
--                         return defaultCallItem
--                     _ ->
--                         -- The only suspect left.
--                         return ci
--
--         runProcM1 course
--
--     raceToFirstFailing :: Script -> IO (Maybe CallItem)
--     raceToFirstFailing Script { config = ScriptConfig { subjects }, call_items } = do
--         init <- newManager tlsManagerSettings
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
-- present lead =
--     let isNonLead = False in -- ??
--     case isNonLead of
--         False -> show lead

        -- let assignments = map (consign mut app) subjects


    -- -- consign :: Base.MVar CallItem -> ProcM () -> String -> IO ()
    -- act io = Base.forkIO $ do
    --     failing <- io
    --     Base.putMVar hole failing

    -- -- Instruct a thread of the IO procedure it will perform.
    -- -- To be caught in race for-loop.
    -- consign :: Base.MVar CallItem -> ProcM () -> String -> IO ()
    -- -- consign :: Base.MVar CallItem -> String -> IO ()
    -- consign failingCallItem flow ratInfo = do
    -- -- consign failingCallItem ratInfo = do
    --     Base.putMVar failingCallItem localhost9999

