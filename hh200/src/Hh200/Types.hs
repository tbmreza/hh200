{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hh200.Types (module Hh200.Types) where

import qualified Data.ByteString       as S8
import qualified Data.HashMap.Strict as HM
import qualified Data.HashTable.IO as H
import qualified Data.ByteString as BS  -- ??: alex ByteString wrapper
import GHC.Generics (Generic)
import Toml.Schema
import Control.Exception (Exception)

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import qualified Data.ByteString.Lazy.Char8 as L8

import Network.HTTP.Types.Header

import Control.Monad
import Control.Monad.Reader

import Network.HTTP.Client
import Network.HTTP.Types.Method
import Control.Monad.Reader
import Control.Monad.Writer
import qualified Data.ByteString.Lazy.Char8 as L8
import Control.Monad.Trans.Maybe
-- import Network.HTTP.Types.Status (statusCode, Status)
import Network.HTTP.Types.Status
import Control.Monad (forM_)

import System.Directory (doesFileExist)
import Control.Exception
import Control.Concurrent (ThreadId)
import qualified Control.Concurrent as Base

newtype Snippet = Snippet L8.ByteString

data DepsClause = DepsClause
  { deps :: [String]
  , itemName :: String
  }
defaultDepsClause = DepsClause { deps = [], itemName = "" }

pCallItem :: DepsClause -> RequestSpec -> Maybe ResponseSpec -> CallItem
pCallItem dc rs opt =
    CallItem
      { ci_deps = deps dc
      , ci_name = itemName dc
      , ci_request_spec = rs
      , ci_response_spec = opt
      }


handleHttpResult :: Either HttpException String -> HttpM ()
handleHttpResult (Right body) =
  liftIO $ putStrLn ("Response: " ++ take 100 body)
handleHttpResult (Left (HttpExceptionRequest _ content)) =
  liftIO $ putStrLn $ "Request failed: " ++ show content
handleHttpResult (Left ex) =
  liftIO $ putStrLn $ "Other HTTP exception: " ++ displayException ex


-- data Script = Script
--   { config :: ScriptConfig
--   , call_items :: [CallItem]
--   } deriving (Show, Eq)
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

-- emptyScript = Script { config = defaultScriptConfig, call_items = [] }

defaultScript :: Script
defaultScript = Script
  { config = defaultScriptConfig
  , callItems = [defaultCallItem]
  }

data RequestSpec = RequestSpec
    { verb :: BS.ByteString  -- ??: v1 of Scanner will use String before jumping to ByteString
    -- { verb :: String
    , url :: String
    , headers :: [String]
    , payload :: String
    , opts :: [String]
    }
    deriving (Show, Eq)

data ResponseSpec = ResponseSpec
  { codes :: [Int]
  , statuses :: [Status]
  , output :: [String]
  }
  deriving (Show, Eq)
defaultResponseSpec :: ResponseSpec
defaultResponseSpec = ResponseSpec { codes = [], output = [] }

type Duration = Int
newtype Subject = Subject String
    deriving (Show, Eq)

data ScriptConfig = ScriptConfig
  { retries :: Int
  , max_duration :: Maybe Duration
  , subjects :: [Subject]
  } deriving (Show, Eq)
defaultScriptConfig :: ScriptConfig
defaultScriptConfig = ScriptConfig { retries = 0, max_duration = Nothing, subjects = [Subject "a", Subject "b"] }

cfgs :: ScriptConfig -> [(String, Maybe String)]
cfgs ScriptConfig {..} =
    [ ("retries", Just $ show retries)
    , ("max_duration", Just "max_duration |> show")
    ]

class PrettyPrint a where
    pp :: a -> String

instance PrettyPrint ScriptConfig where
    --        as concrete script header  #! retries 1
    --                                   #! max-duration 1m
    -- cfg in cfgs, map "#! {cfg.0} {cfg.1}"
    pp x = show x

data CallItem = CallItem
  { ci_deps :: [String]
  , ci_name :: String
  , ci_request_spec :: RequestSpec
  , ci_response_spec :: Maybe ResponseSpec
  } deriving (Show, Eq)

-- Mechanically, this is a corollary to http-client's defaultRequest.
--
-- "A default request value, a GET request of localhost/:80, with an empty
-- request body." - http-client hoogle
defaultCallItem :: CallItem
defaultCallItem = CallItem
  { ci_deps = []
  , ci_name = "default"
  , ci_request_spec = RequestSpec
    { verb = "GET"
    , url = "http://localhost:80"
    , headers = [], payload = "", opts = []
    }
  , ci_response_spec = Nothing
  }

localhost9999 :: CallItem
localhost9999 = CallItem
  { ci_deps = []
  , ci_name = "debug"
  , ci_request_spec = RequestSpec { verb = "GET", url = "http://localhost:9999/hh", headers = [], payload = "", opts = []}
  , ci_response_spec = Nothing
  }

-- Zero or more HTTP effects ready to be run by `runHttpM`.
stackHh :: [CallItem] -> HttpM L8.ByteString
stackHh [CallItem { ci_deps, ci_name, ci_request_spec = RequestSpec { verb, url }, ci_response_spec = Nothing }] = do
    mkRequest verb url Nothing

stackHh [CallItem { ci_request_spec = RequestSpec { verb, url }, ci_response_spec = Just ResponseSpec { codes } }] = do
    mkRequest verb url Nothing


-- -- Pretty printing conveniently presents counter-example to std out.
-- instance PrettyPrint CallItem where
--     pp CallItem { ci_request_spec } = conc ci_request_spec

-- ??: host computer info, /etc/resolv.conf, execution time
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
  | BareLead
      { firstFailing :: Maybe CallItem
      , hostInfo ::     HostInfo
      , echoScript ::   Maybe Script
      }
  | NonLead
      { firstFailing :: Maybe CallItem
      , hostInfo ::     HostInfo
      , echoScript ::   Maybe Script
      }
  deriving (Show, Eq)

nonLead :: Script -> Lead
nonLead x = NonLead
  { firstFailing = Nothing
  , echoScript = Just x
  }

leadFrom :: Maybe CallItem -> Script -> Lead
leadFrom failed script = Lead
  { firstFailing = failed
  , hostInfo = defaultHostInfo
  , echoScript = Just script
  }

-- gatherHostInfo :: IO HostInfo

bareLeadWith :: Log -> Lead
bareLeadWith x = BareLead {}

bareLead :: Lead
bareLead = BareLead
  { firstFailing = Just CallItem
    { ci_deps = []
    , ci_name = ""
    , ci_response_spec = Nothing
    , ci_request_spec = RequestSpec
      { verb = "GET"
      , url = "example.com"
      , headers = []
      , payload = ""
      , opts = []
      }
    }
  }

-- These rats compete with each other for the first counter-example or `Lead`.
-- At the end of the race, all rats die; "rat race".
data Rat = Rat

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

httpGet_ :: String -> HttpM ()
httpGet_ url = do
    manager <- ask
    request <- liftIO $ parseRequest url
    _response <- liftIO $ httpLbs request manager
    return ()

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
type ExpectCode = Int
type Url = String

-- setRequestHeader :: H.HeaderName -> [BS.ByteString] -> H.Request -> H.Request
-- let hlInput = setRequestHeader "Content-Type" ["application/x-yaml"] $ ""

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

-- instance FromValue Policy where fromValue = genericFromTable
instance ToTable Policy where toTable = genericToTable
instance ToValue Policy where toValue = defaultTableToValue

policyDefault :: Policy
-- policyDefault = Policy { maxReruns = 2, maxRetriesPerCall = 2, timeMillisPerCall = 60_000 }
policyDefault = Policy { maxReruns = Just 2, maxRetriesPerCall = Just 2, timeMillisPerCall = Just 60000 }

instance FromValue Policy where
    fromValue = parseTableFromValue (Policy
        <$> optKey "max_reruns"
        <*> optKey "max_retries_per_call"
        <*> optKey "time_millis_per_call"
        )

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
    return (defaultCallItem, [])

runProcM1 :: ProcM CallItem -> IO (Maybe CallItem, Log)
runProcM1 action = do
    manager <- newManager tlsManagerSettings
    runReaderT (runWriterT (runMaybeT action)) manager


-- runRaceM :: ProcM a -> IO (Maybe a, Log)
runRaceM :: ProcM CallItem -> IO (Maybe CallItem, Log)
runRaceM action = do
    manager <- newManager tlsManagerSettings
    runReaderT (runWriterT (runMaybeT action)) manager


shuntGet1 :: String ->                  ProcM L8.ByteString
shuntGet1 = shuntHttpRequest (\req -> req { method = "GET" })

shuntPost :: L8.ByteString -> String -> ProcM L8.ByteString
shuntPost body = shuntHttpRequest (\req -> req
  { method = "POST"
  , requestBody = RequestBodyLBS body
  , requestHeaders = ("Content-Type", "application/json") : requestHeaders req
  })

shuntGet :: String -> ProcM L8.ByteString
shuntGet url = do
  logMsg $ "Requesting: " ++ url
  manager <- lift . lift $ ask
  result <- liftIO $ try $ do
    req <- parseRequest url
    httpLbs req manager
  case result of
    Right resp -> do
      let status = statusCode $ responseStatus resp
      logMsg $ "Received status: " ++ show status
      if status >= 200 && status < 300
        then return $ responseBody resp
        else do
          logMsg $ "Non-success status: " ++ show status
          MaybeT $ return Nothing
    Left (HttpExceptionRequest _ content) -> do
      logMsg $ "HttpExceptionRequest: " ++ show content
      MaybeT $ return Nothing
    Left err -> do
      logMsg $ "Other exception: " ++ displayException err
      MaybeT $ return Nothing

shuntHttpRequestFull :: Request -> ProcM (Response L8.ByteString)
shuntHttpRequestFull req = do
    -- manager <- lift . lift $ ask
    liftIO $ putStrLn "shuntHttpRequestFull....."
    liftIO $ putStrLn (show req)
    mgr :: Manager <- ask
    result <- liftIO $ try (httpLbs req mgr)
    case result of
        Left err -> do
            tell ["HTTP error: " ++ show (err :: HttpException)]
            MaybeT $ return Nothing
        Right body -> return body

shuntHttpRequest :: (Request -> Request) -> String -> ProcM L8.ByteString
shuntHttpRequest modifyReq url = do
  manager <- lift . lift $ ask
  result <- liftIO $ try $ do
    initReq <- parseRequest url
    let req = modifyReq initReq
    response <- httpLbs req manager
    return $ responseBody response
  case result of
    Left err -> do
      tell ["HTTP error: " ++ show (err :: HttpException)]
      MaybeT $ return Nothing
    Right body -> return body


-- App is procedure in a stack that will return a failing CallItem.
app :: ProcM CallItem
app = do
    logMsg $ "Requesting: " ++ "url"

    -- body <- shuntGet "https://httpbin.org/get"
    body <- shuntGet "http://localhost:9999/oo"

    -- logMsg $ "Got response of length: " ++ show (L8.length body)
    -- liftIO $ putStrLn $ "Response preview: " ++ take 100 (L8.unpack body)
    -- liftIO circuit
    liftIO $ return localhost9999

-- { method = verb $ ci_request_spec defaultCallItem  -- ??: handle lower case method user input
-- Course is procedure in a stack form that will return the
-- CallItem that turned out to be failing.
courseFrom :: Script -> ProcM CallItem
courseFrom x = do
    -- -- build :: Request <- parseRequest (urlFrom x)
    -- build :: Request <- parseRequest (url $ ci_request_spec defaultCallItem)
    -- let struct :: Request = build
    --       { method = verb $ ci_request_spec defaultCallItem
    --       }
    --
    -- -- inst :: Response L8.ByteString <- oneResponse $ oneRequest x
    -- inst :: Response L8.ByteString <- shuntHttpRequestFull struct

    req <- liftIO (oneRequest x)
    res :: Response L8.ByteString <- oneResponse req

    let [ci] = callItems x
    let expectCodes :: [Status] = case ci_response_spec ci of
            Nothing -> [status200]
            Just spec -> statuses spec

    return $ case True of
        False -> ci

    where
    oneRequest :: Script -> IO Request
    oneRequest (Script { callItems = [ci]}) = do
        let scriptUrl = url $ ci_request_spec ci
        build :: Request <- parseRequest scriptUrl
        -- let struct :: Request = build
        --       { method = verb $ ci_request_spec defaultCallItem
        --       }
        return build

    oneResponse :: Request -> ProcM (Response L8.ByteString)
    oneResponse req = do
        mgr :: Manager <- ask
        result <- liftIO $ try (httpLbs req mgr)
        case result of
            Left err -> do
                tell ["HTTP error: " ++ show (err :: HttpException)]
                MaybeT $ return Nothing
            Right body -> return body

testOutsideWorld :: Script -> IO Lead

-- -> NonLead
testOutsideWorld static@(Script { config, callItems = [] }) = do
    return $ nonLead static

-- -> NonLead | BareLead | Lead
testOutsideWorld sole@(Script { config = ScriptConfig { subjects }, callItems = [ci] }) = do
    let reconciled = sole

    let course :: ProcM CallItem = courseFrom sole
    failed :: (Maybe CallItem, Log) <- runProcM1 course
    return $ case failed of
        (Nothing, []) ->     nonLead sole
        (Nothing, logs) ->   bareLead
        (opt@(Just _), _) -> leadFrom opt sole

    -- -- PICKUP
    -- let course :: ProcM CallItem = courseFrom sole
    -- -- failed <- runProcM course
    -- -- return $ case failed of
    -- --     (Nothing, []) ->     nonLead sole
    -- --     (Nothing, logs) ->   bareLead
    -- --     (opt@(Just _), _) -> leadFrom opt sole

-- -> NonLead | BareLead | Lead
testOutsideWorld script@(Script { callItems }) = do
    return bareLead

-- old is Return Nothing if it can't even compile a bareLead.

-- testOutsideWorld1 :: Script -> MaybeT IO Lead
--
-- testOutsideWorld1 Script { config, call_items = [] } = do
--     MaybeT (return $ Just Lead { firstFailing = Nothing })
--
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
--                 build :: Request <- parseRequest (url $ ci_request_spec ci)
--                 let struct = build
--                       -- { method = verb $ ci_request_spec ci  -- ??: handle lower case method user input
--                       { method = verb $ ci_request_spec ci
--                       }
--
--                 ret <- shuntHttpRequestFull struct
--
--                 let expectCodes = case ci_response_spec ci of
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
--
-- -- testOutsideWorld1 (Script { config = ScriptConfig { subjects }, call_items }) = do
-- testOutsideWorld1 single@(Script { config = ScriptConfig { subjects }, call_items }) = do
--     MaybeT (return $ Just Lead { firstFailing = Nothing })
--
-- -- testOutsideWorld1 _unexpected = MaybeT $ return Nothing

present :: Lead -> String
present lead = show lead

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

