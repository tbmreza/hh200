{-# Language DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}

module Hh200.Types where

import qualified Data.ByteString       as S8
import qualified Data.HashMap.Strict as HM
import qualified Data.HashTable.IO as H
import qualified Data.ByteString as S
import GHC.Generics (Generic)
import Toml.Schema
import Network.HTTP.Types.Header (RequestHeaders, HeaderName)
import Control.Exception (Exception, throwIO)

-- mod Cl {
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status (statusCode)
import qualified Data.ByteString.Lazy.Char8 as L8

import Network.HTTP.Types.Status
import Network.HTTP.Types.Header

import Control.Monad.Reader

-- draft {

data Callable = Callable {
      deps :: [String]
    , name :: String
    , request :: Req
    , response :: Resp
    , been_called :: Bool
    , err_stack :: [String]
    }
    deriving (Show, Eq)

data Req = Req {
      method :: String
    , url :: String
    , headers :: [String]
    , payload :: String
    , opts :: [String]
    }
    deriving (Show, Eq)

data Resp = Resp {
      codes :: [Int]
    , output :: [String]
    }
    deriving (Show, Eq)

data Mini = Mini {
      m_url :: String
    }

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

doPerform :: Rat -> String -> IO ()
doPerform _ url = do
    -- manager <- newManager tlsManagerSettings
    manager <- acquire
    request <- parseRequest url

    -- Perform the request
    response <- httpLbs request manager

    -- Extract response details
    let status = responseStatus response
        headers = responseHeaders response
        body = responseBody response

    putStrLn $ "Status code: " ++ show (statusCode status)
    putStrLn $ "Response body: " ++ L8.unpack body

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
    response <- liftIO $ httpLbs request manager
    return ()

httpGet :: String -> HttpM L8.ByteString
httpGet url = do
    manager <- ask
    request <- liftIO $ parseRequest url
    response <- liftIO $ httpLbs request manager
    return $ responseBody response

httpPost :: String -> L8.ByteString -> HttpM L8.ByteString
httpPost url jsonBody = do
    manager <- ask
    initialRequest <- liftIO $ parseRequest url
    
    let request = initialRequest
            { method = "POST"
            , requestBody = RequestBodyLBS jsonBody
            , requestHeaders = [("Content-Type", "application/json")]
            }
    
    response <- liftIO $ httpLbs request manager
    return $ responseBody response

-- seqCl :: HttpM ()
-- seqCl = do
--     postResp <- httpPost "https://httpbin.org/post" "{\"reader\": \"monad\"}"
--     liftIO $ putStrLn $ "POST response: " ++ take 100 (L8.unpack postResp)

-- }

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
