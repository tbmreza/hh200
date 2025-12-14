{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hh200.Types
where
  -- ( HostInfo(..), defaultHostInfo
  -- , RequestSpec(..)
  -- , ResponseSpec(..)
  -- , DepsClause(..), defaultDepsClause
  -- , Script(..), ScriptConfig(..), defaultScriptConfig, dbgScriptConfig
  -- , Snippet(..)
  -- , pCallItem, CallItem(..), firstFailing, callItemIsDefault
  -- , UppercaseString, expectUpper, showVerb
  -- , module Network.HTTP.Types.Status
  -- , Binding
  -- , RhsDict(..)
  -- , show'
  -- , present, noNews
  -- , oftenBodyless
  -- , Env
  -- , Log
  -- ) where

import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Function (on)
import qualified Data.HashMap.Strict as HM
import           Data.List (sortBy)
import qualified Data.List.NonEmpty as Ls (NonEmpty(..))
import           Data.Text (Text)
import qualified Data.Text as Text

import           Network.HTTP.Types.Status

import qualified BEL


--------------------------------------------------------------------------------
-- Type Aliases & Newtypes
--------------------------------------------------------------------------------

newtype UppercaseString = UppercaseString String
    deriving (Show, Eq)

newtype Snippet = Snippet L8.ByteString

newtype RhsDict = RhsDict (HM.HashMap String [BEL.Part])
    deriving (Show, Eq)

mtHM :: HM.HashMap k v
mtHM = HM.empty

type Duration = Int

newtype Subject = Subject String
    deriving (Show, Eq)

type Binding = (String, [BEL.Part])

type Env = BEL.Env -- = HM.HashMap String Aeson.Value

data TraceEvent
  = ScriptStart Int
  | ItemStart String
  | HttpError String
  | HttpStatus Int
  | CapturesStart Int
  | Captured String
  | AssertsFailed
  | AssertsPassed
  deriving (Show, Eq)

type Log = [TraceEvent]


--------------------------------------------------------------------------------
-- Core Data Types
--------------------------------------------------------------------------------

data ScriptKind = Regular | Static | Sole
    deriving (Show, Eq)

data Script = Script
  { kind :: ScriptKind
  , config :: ScriptConfig
  , callItems :: [CallItem]
  }
  deriving (Show, Eq)

data ScriptConfig = ScriptConfig
  { retries :: Int
  , maxDuration :: Maybe Duration
  , subjects1 :: [Subject]
  , subjects :: Ls.NonEmpty Subject
  , useTls :: Bool
  } deriving (Show, Eq)

data DepsClause = DepsClause
  { deps :: [String]
  , itemName :: String
  }

data CallItem = CallItem
  { ciDeps :: [String]
  , ciName :: String
  , ciRequestSpec :: RequestSpec
  , ciResponseSpec :: Maybe ResponseSpec
  } deriving (Show, Eq)

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

-- gatherHostInfo :: IO HostInfo

-- Host computer info: /etc/resolv.conf, execution time,
data HostInfo = HostInfo
  { hiUptime ::    Maybe String
  , hiHh200Conf :: Maybe ScriptConfig
  } deriving (Show, Eq)

data LeadKind = Normal | Debug | Non
    deriving (Show, Eq)

-- Everything one could ask for when debugging a failing script.
-- ??: pct field to append to .dat
data Lead = Lead
  { leadKind ::        LeadKind
  , firstFailing ::    Maybe CallItem
  , hostInfo ::        HostInfo
  , interpreterInfo :: (Env, Log)
  , echoScript ::      Maybe Script
  } deriving (Show)

data InternalError = OutOfBounds
                   | Todo
    deriving (Show, Eq)

data HhError = LibError
             | SystemError
             | PointableError
    deriving (Show)


--------------------------------------------------------------------------------
-- Defaults & Smart Constructors
--------------------------------------------------------------------------------

defaultScriptConfig :: ScriptConfig
defaultScriptConfig = ScriptConfig
  { retries = 0
  , maxDuration = Nothing
  , subjects1 = [Subject "default a"]
  , subjects = (Subject "a") Ls.:| []
  , useTls = True
  }

dbgScriptConfig :: ScriptConfig
dbgScriptConfig = ScriptConfig
  { retries = 0
  , maxDuration = Nothing
  , subjects1 = [Subject "custommm"]
  , subjects = (Subject "a") Ls.:| []
  , useTls = True
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

callItemIsDefault :: CallItem -> Bool
callItemIsDefault CallItem { ciName } = ciName == "default"

defaultHostInfo :: HostInfo
defaultHostInfo = HostInfo
  { hiUptime = Nothing
  , hiHh200Conf = Nothing
  }


--------------------------------------------------------------------------------
-- Small Helpers / Utilities
--------------------------------------------------------------------------------

show' :: Text -> String
show' t = trimQuotes $ show t

trimQuotes :: String -> String
trimQuotes s =
  case s of
    ('"':xs) -> case reverse xs of
                  ('"':ys) -> reverse ys
                  _        -> s
    _        -> s

-- | __Partial__: Asserts uppercase input.
expectUpper :: String -> UppercaseString
expectUpper s | all (`elem` ['A'..'Z']) s = UppercaseString s
expectUpper _ = undefined

showVerb :: UppercaseString -> String
showVerb (UppercaseString s) = s

oftenBodyless :: UppercaseString -> Bool
oftenBodyless (UppercaseString s) = elem s ["GET", "HEAD", "OPTIONS", "TRACE"]

noNews :: Lead -> Bool
noNews (Lead { leadKind = Non }) = True
noNews _ = False

present :: CallItem -> String
present ci = (showVerb $ verb $ ciRequestSpec ci) ++ " " ++ (url $ ciRequestSpec ci)
 ++ (showHeaders $ headers $ ciRequestSpec ci)
 -- ++ "\n" ++ (payload $ ciRequestSpec ci)
 ++ (payload $ ciRequestSpec ci)
 ++ (showResponse $ ciResponseSpec ci) ++ "\n"

showHeaders :: RhsDict -> String
showHeaders (RhsDict hm) = concatMap fmt $ sortBy (compare `on` fst) $ HM.toList hm
  where
    fmt (k, v) = "\n" ++ k ++ ": " ++ unwords (map showPart v)

showPart :: BEL.Part -> String
showPart (BEL.R t) = Text.unpack t
showPart (BEL.L _) = "???"

showResponse :: Maybe ResponseSpec -> String
showResponse Nothing = ""
showResponse (Just rs) = "\nHTTP " ++ (unwords $ map (show . statusCode) (statuses rs))

data Dat = Dat [(Text, Text, Text)]
    deriving (Show)
