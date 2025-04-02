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

type Source = FilePath  -- ??: ending with .hhs

type HttpVerb = S.ByteString

type HashTable k v = H.BasicHashTable k v
type Headers = HashTable String String
type ExpectCode = Int
type Url = String

-- data Instr = NOP
--            | IV
--            | IC
--            | OV HttpVerb
--            | OC ExpectCode
--            | SU Url
--            | SH HeaderName [S.ByteString]
--            | X
--            | MATCH_CODES Int
--     deriving (Show, Eq)

-- setRequestHeader :: H.HeaderName -> [S.ByteString] -> H.Request -> H.Request
-- let hlInput = setRequestHeader "Content-Type" ["application/x-yaml"] $ ""

type Vars = HM.HashMap String Integer

varsEmpty = HM.empty
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
