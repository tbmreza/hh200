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
import qualified Data.ByteString as S
import GHC.Generics (Generic)
import Toml.Schema
import Control.Exception (Exception)

-- mod Cl {
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import qualified Data.ByteString.Lazy.Char8 as L8

import Network.HTTP.Types.Header

import Control.Monad
import Control.Monad.Reader
-- }

-- draft {
import Network.HTTP.Client
import Network.HTTP.Types.Method
import Control.Monad.Reader
import Control.Monad.Writer
import qualified Data.ByteString.Lazy.Char8 as L8
import Control.Monad.Trans.Maybe
import Network.HTTP.Types.Status (statusCode)
import Control.Monad (forM_)

import System.Directory (doesFileExist)
import Control.Exception
import Control.Concurrent (ThreadId)
import qualified Control.Concurrent as Base

-- shuntGetHttpM :: String -> HttpM (Either HttpException String)
-- shuntGetHttpM url = do
--   manager <- ask
--   result <- liftIO $ try $ do
--     request <- parseRequest url
--     response <- httpLbs request manager
--     return (responseBody response)
--   return $ fmap show result

handleHttpResult :: Either HttpException String -> HttpM ()
handleHttpResult (Right body) =
  liftIO $ putStrLn ("Response: " ++ take 100 body)
handleHttpResult (Left (HttpExceptionRequest _ content)) =
  liftIO $ putStrLn $ "Request failed: " ++ show content
handleHttpResult (Left ex) =
  liftIO $ putStrLn $ "Other HTTP exception: " ++ displayException ex


data Script = Script
  { config :: ScriptConfig
  , call_items :: [CallItem]
  } deriving (Show, Eq)

emptyScript = Script { config = defaultScriptConfig, call_items = [] }

localhost9999Script :: Script
localhost9999Script = Script { config = defaultScriptConfig, call_items = [localhost9999] }

data RequestSpec = RequestSpec
    { verb :: S.ByteString
    , url :: String
    , headers :: [String]
    , payload :: String
    , opts :: [String]
    }
    deriving (Show, Eq)

conc :: RequestSpec -> String
conc _ = "POST http://localhost:9999/user\n{ \"name\": \"johk\" }"

data ResponseSpec = ResponseSpec {
      codes :: [Int]
    , output :: [String]
    }
    deriving (Show, Eq)
defaultResponseSpec :: ResponseSpec
defaultResponseSpec = ResponseSpec { codes = [], output = [] }

type Duration = Int
data ScriptConfig = ScriptConfig
  { retries :: Int
  , max_duration :: Maybe Duration
  , subjects :: [String]
  } deriving (Show, Eq)
defaultScriptConfig :: ScriptConfig
defaultScriptConfig = ScriptConfig { retries = 0, max_duration = Nothing, subjects = [] }

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

localhost9999 :: CallItem
localhost9999 = CallItem
  { ci_deps = []
  , ci_name = "debug"
  , ci_request_spec = RequestSpec { verb = "GET", url = "http://localhost:9999/hh", headers = [], payload = "", opts = []}
  , ci_response_spec = Nothing
  }

dbgUrl :: CallItem -> String
dbgUrl CallItem { ci_request_spec = RequestSpec { url } } = url

-- Zero or more HTTP effects ready to be run by `runHttpM`.
stackHh :: [CallItem] -> HttpM L8.ByteString
stackHh [CallItem { ci_deps, ci_name, ci_request_spec = RequestSpec { verb, url }, ci_response_spec = Nothing }] = do
    mkRequest verb url Nothing

stackHh [CallItem { ci_request_spec = RequestSpec { verb, url }, ci_response_spec = Just ResponseSpec { codes } }] = do
    mkRequest verb url Nothing


-- Pretty printing conveniently presents counter-example to std out.
instance PrettyPrint CallItem where
    pp CallItem { ci_request_spec } = conc ci_request_spec


data Lead = Lead
  { firstFailing :: Maybe CallItem
  -- , top :: Top  -- ??: host computer info, /etc/resolv.conf, execution time
  } deriving (Show, Eq)

basicLead :: Lead
basicLead = Lead
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

-- }

-- acquire :: IO Manager
-- acquire = newManager tlsManagerSettings

-- These rats compete with each other for the first counter-example or `Lead`.
-- At the end of the race, all rats die; "rat race".
data Rat = Rat

-- doOrder :: Rat -> String -> IO ()
-- doOrder _ url = do
--     manager <- acquire
--     -- initialRequest <- parseRequest "POST https://httpbin.org/post"
--     initialRequest <- parseRequest url
--     
--     -- JSON payload
--     let jsonBody = "{\"name\": \"John\", \"age\": 30}"
--     
--     -- Modify request with body and headers
--     let request = initialRequest
--             { method = "POST"
--             , requestBody = RequestBodyLBS (L8.pack jsonBody)
--             , requestHeaders = 
--                 [ (hContentType, "application/json")
--                 , (hAccept, "application/json")
--                 ]
--             }
--     
--     response <- httpLbs request manager
--     
--     putStrLn $ "Status: " ++ show (responseStatus response)
--     putStrLn $ "Body: " ++ L8.unpack (responseBody response)

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

mkRequest :: S.ByteString -> String -> Maybe L8.ByteString -> HttpM L8.ByteString
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

-- httpGet_ :: String -> HttpM L8.ByteString
-- httpGet_ url = mkRequest "GET" url Nothing

httpGet :: String -> HttpM L8.ByteString
httpGet url = mkRequest "GET" url Nothing

httpPost :: String -> L8.ByteString -> HttpM L8.ByteString
httpPost url body = mkRequest "POST" url (Just body)


-- httpGet :: String -> HttpM L8.ByteString
-- httpGet url = do
--     manager <- ask
--     request <- liftIO $ parseRequest url
--     response <- liftIO $ httpLbs request manager
--     return $ responseBody response
--
-- httpPost :: String -> L8.ByteString -> HttpM L8.ByteString
-- httpPost url jsonBody = do
--     manager <- ask
--     initialRequest <- liftIO $ parseRequest url
--     
--     let request = initialRequest
--             { method = "POST"
--             , requestBody = RequestBodyLBS jsonBody
--             , requestHeaders = [("Content-Type", "application/json")]
--             }
--     
--     response <- liftIO $ httpLbs request manager
--     return $ responseBody response

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

type HttpVerb = S.ByteString

type HashTable k v = H.BasicHashTable k v
type Headers = HashTable String String
type ExpectCode = Int
type Url = String

-- setRequestHeader :: H.HeaderName -> [S.ByteString] -> H.Request -> H.Request
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
-- terminating:  staticChecks  testOutsideWorld  present
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

runRaceM :: ProcM a -> IO (Maybe a, Log)
runRaceM action = do
    manager <- newManager tlsManagerSettings
    runReaderT (runWriterT (runMaybeT action)) manager
    -- return (Just localhost9999, [])
    -- return (Nothing, [])


shuntGet1 :: String ->                  ProcM L8.ByteString
shuntGet1 = shuntHttpRequest (\req -> req { method = "GET" })

shuntPost :: L8.ByteString -> String -> ProcM L8.ByteString
shuntPost body = shuntHttpRequest (\req -> req
  { method = "POST"
  , requestBody = RequestBodyLBS body
  , requestHeaders = ("Content-Type", "application/json") : requestHeaders req
  })

-- builder :: [CallItem]

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

-- draft general shuntFire
shuntHttpRequestFull :: Request ->      ProcM L8.ByteString
shuntHttpRequestFull req = do
  manager <- lift . lift $ ask
  result <- liftIO $ try $ httpLbs req manager
  case result of
    Left err -> do
      tell ["HTTP error: " ++ show (err :: HttpException)]
      MaybeT $ return Nothing
    Right body -> return $ responseBody body


-- draft general shuntFire
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
    -- body <- shuntGet "https://httpbin.org/get"
    -- logMsg $ "Got response of length: " ++ show (L8.length body)
    -- liftIO $ putStrLn $ "Response preview: " ++ take 100 (L8.unpack body)
    -- liftIO circuit
    liftIO $ return localhost9999


staticChecks :: FilePath -> MaybeT IO Script
staticChecks path = do
  exists <- liftIO $ doesFileExist path
  if exists
    then liftIO $ return localhost9999Script
    else MaybeT $ return Nothing

circuit :: IO ()
circuit = do
    return ()
  -- (result, logs) <- runRaceM app
  -- putStrLn "\n--- Logs ---"
  -- mapM_ putStrLn logs
  -- case result of
  --   Just _  -> putStrLn "✅ Success"
  --   Nothing -> putStrLn "❌ Failed"


testOutsideWorld :: Script -> MaybeT IO Lead

testOutsideWorld Script { config, call_items = [] } = do
    MaybeT (return $ Just Lead { firstFailing = Nothing })

testOutsideWorld Script { config = ScriptConfig { subjects }, call_items = [ci] } = do
    -- manager <- liftIO $ newManager tlsManagerSettings
    coming <- liftIO $ raceToFirstFailing Script { config = ScriptConfig { subjects }, call_items = [ci] }

    -- -- ??: buildRequest
    -- (maybeResult, logs) <- liftIO $ runReaderT (runWriterT (runMaybeT (shuntGet $ dbgUrl ci))) manager

    MaybeT (return $ Just Lead { firstFailing = coming })

    where
    raceToFirstFailing :: Script -> IO (Maybe CallItem)
    raceToFirstFailing Script { config = ScriptConfig { subjects } } = do
        manager <- newManager tlsManagerSettings
        hole <- Base.newEmptyMVar

        -- let proc = shuntGet $ dbgUrl ci
        -- (maybeResult, logs) <- runReaderT (runWriterT (runMaybeT (proc))) manager
        let actions :: [IO (Maybe CallItem, Log)] = [runRaceM app]

        tids :: [ThreadId] <- forM actions $
            -- Each action returns a thread ID.
            \(io :: IO (Maybe CallItem, Log)) -> Base.forkIO $ do
                (res, _log) <- io

                case res of
                    Just failing -> Base.putMVar hole failing
                    Nothing -> return ()

        forM_ tids $ \id -> Base.killThread id
        Base.tryReadMVar hole

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

testOutsideWorld _unexpected = MaybeT $ return Nothing

present :: Lead -> String
present lead = show lead
