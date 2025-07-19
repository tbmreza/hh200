{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}

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

import Control.Monad.Reader
-- }

-- draft {
import Network.HTTP.Client
import Network.HTTP.Types.Method (Method)
import Control.Monad.Reader
import qualified Data.ByteString.Lazy.Char8 as L8


data Script = Script
  { config :: ScriptConfig
  , call_items :: [CallItem]
  }

data RequestSpec = RequestSpec {
      method :: String
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

-- Fields are optional unless a default value makes sense.
type Duration = Int
data ScriptConfig = ScriptConfig { retries :: Int, max_duration :: Maybe Duration, subjects :: [String] }
    deriving (Show)
defaultScriptConfig :: ScriptConfig
defaultScriptConfig = ScriptConfig { retries = 0, max_duration = Nothing, subjects = [] }

cfgs :: ScriptConfig -> [(String, Maybe String)]
cfgs ScriptConfig {..} =
    [ ("retries", Just $ show retries)
    , ("max_duration", Just "??: max_duration |> show")
    ]

class PrettyPrint a where
    pp :: a -> String

instance PrettyPrint ScriptConfig where
    -- ??:    as concrete script header  #! retries 1
    --                                   #! max-duration 1m
    -- cfg in cfgs, map "#! {cfg.0} {cfg.1}"
    pp x = show x

data CallItem = CallItem
  { ci_deps :: [String]
  , ci_name :: String
  , ci_request_spec :: RequestSpec
  , ci_response_spec :: Maybe ResponseSpec
  } deriving (Show)
-- defaultCallItem :: CallItem
-- defaultCallItem = CallItem { ci_request_spec = defaultRequestSpec, ci_response_spec = Nothing }
-- defaultCallItem = CallItem { ci_request_spec = defaultRequestSpec }

-- Zero or more HTTP effects ready to be run by `runHttpM`.
-- ??: whether TH is the right abstraction somewhere
stackHh :: [CallItem] -> HttpM L8.ByteString
stackHh [CallItem { ci_deps, ci_name, ci_request_spec = RequestSpec { method, url } }] = do
    httpGet url

-- Pretty printing conveniently presents counter-example to std out.
-- ?? callsites: rat exceptions, debug echo config
instance PrettyPrint CallItem where
    pp CallItem { ci_request_spec } = conc ci_request_spec


data Lead = Lead
  { c :: CallItem
  -- , top :: Top  -- ??: host computer info
  } deriving (Show)

basicLead :: Lead
basicLead = Lead
  { c = CallItem
    { ci_deps = []
    , ci_name = ""
    , ci_response_spec = Nothing
    , ci_request_spec = RequestSpec
      { method = "GET"
      , url = "example.com"
      , headers = []
      , payload = ""
      , opts = []
      }
    }
  }

-- Callable, DNS configs (??: /etc/resolv.conf), Execution time,
-- instance Show Lead where
--     show (Lead (Ckallable m u) v) =
--         show m ++ "\n" ++ show u ++ "\n"

-- }

acquire :: IO Manager
acquire = newManager tlsManagerSettings

-- These rats compete with each other for the first counter-example or `Lead`.
-- At the end of the race, all rats die; "rat race".
data Rat = Rat

doOrder :: Rat -> String -> IO ()
doOrder _ url = do
    manager <- acquire
    -- initialRequest <- parseRequest "POST https://httpbin.org/post"
    initialRequest <- parseRequest url
    
    -- JSON payload
    let jsonBody = "{\"name\": \"John\", \"age\": 30}"
    
    -- Modify request with body and headers
    let request = initialRequest
            { method = "POST"
            , requestBody = RequestBodyLBS (L8.pack jsonBody)
            , requestHeaders = 
                [ (hContentType, "application/json")
                , (hAccept, "application/json")
                ]
            }
    
    response <- httpLbs request manager
    
    putStrLn $ "Status: " ++ show (responseStatus response)
    putStrLn $ "Body: " ++ L8.unpack (responseBody response)

type HttpM = ReaderT Manager IO
-- Presume http-client manager sharing.
runHttpM :: HttpM a -> IO a
runHttpM action = do
    manager <- newManager tlsManagerSettings
    runReaderT action manager

httpGet_ :: String -> HttpM ()
httpGet_ url = do
    manager <- ask
    request <- liftIO $ parseRequest url
    _response <- liftIO $ httpLbs request manager
    return ()

mkRequest :: Method -> String -> Maybe L8.ByteString -> HttpM L8.ByteString
mkRequest    method    url       mBody                = do
    manager <- ask
    initialRequest <- liftIO $ parseRequest url
    let request = initialRequest
            { method = method
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
