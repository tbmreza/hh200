{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hh200.Types
    ( UpperString (..)
    , Snippet (..)
    , RhsDict (..)
    , mtHM
    , Duration
    , Subject (..)
    , Binding
    , Env
    , TraceEvent (..)
    , Log
    , ScriptKind (..)
    , Script (..)
    , Scriptg (..)
    , ScriptConfig (..)
    , DepsClause (..)
    , CallItem (..)
    -- , CallItemg (..)
    , RequestSpec (..)
    , ResponseSpec (..)
    , HostInfo (..)
    , LeadKind (..)
    , Lead (..), Leadg (..)
    , InternalError (..)
    , HhError (..)
    , defaultScriptConfig
    , effectiveTls
    , defaultDepsClause
    -- , pCallItem
    , gCallItem
    -- , callItemIsDefault
    , defaultHostInfo
    , defaultLead
    , show'
    , trimQuotes
    , expectUpper, expectUrl
    , showVerb, showUrl
    , noNews
    , present
    , showHeaders
    , showPart
    , showResponse
    , Dat (..)
    ) where

import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Function (on)
import qualified Data.HashMap.Strict as HM
import           Data.List (sortBy)
import qualified Data.List.NonEmpty as Ls (NonEmpty(..))
import           Data.Text (Text)
import qualified Data.Text as Text

import qualified Network.HTTP.Client as HC
import           Network.HTTP.Types.Status
import           Network.URI (parseURI, uriScheme)

import qualified BEL


newtype UpperString = UpperString String
    deriving (Show, Eq)

-- | __Partial__: Asserts uppercase input.
expectUpper :: String -> UpperString
expectUpper s | all (`elem` ['A'..'Z']) s = UpperString s
expectUpper _ = undefined

-- newtype UrlString = UrlString requestStruct
newtype UrlString = UrlString String
    deriving (Show, Eq)

expectUrl :: String -> UrlString
expectUrl s = UrlString s
-- expectUrl _ = undefined

newtype Snippet = Snippet L8.ByteString

-- Cookie is a native concept in BEL.
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


data ScriptKind = Regular | Static | Sole
    deriving (Show, Eq)

data Scriptg = Scriptg
  { kindg :: ScriptKind
  , configg :: ScriptConfig
  , callItemsg :: [CallItem]
  } deriving (Show)

data Script = Script
  { kind :: ScriptKind
  , config :: ScriptConfig
  , callItems :: [CallItem]
  -- } deriving (Show, Eq)
  } deriving (Show)

-- ??: ScriptConfig definition and its derivatives mean very little until some
-- of them are used in one of Execution modes.
data ScriptConfig = ScriptConfig
  { retries :: Int
  , maxDuration :: Maybe Duration
  , subjects :: Ls.NonEmpty Subject
  , useTls :: Maybe Bool
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
  } deriving (Show)

data RequestSpec = RequestSpec
  { requestStruct :: Maybe HC.Request
  , method ::        String
  , lexedUrl ::      String
  , headersg ::      RhsDict
  , configsg ::      RhsDict
  , payloadg ::      String
  } deriving (Show)

data ResponseSpec = ResponseSpec
  { statuses :: [Status]
  , output :: [String]
  , captures :: RhsDict
  , asserts :: [String]  -- List of untyped expr line, input for evaluator.
  }
  deriving (Show, Eq)

-- Host computer info: /etc/resolv.conf, execution time,
data HostInfo = HostInfo
  { hiHh200Conf ::   Maybe ScriptConfig
  , hiHostname ::    Maybe String
  , hiOs ::          Maybe String
  , hiArch ::        Maybe String
  , hiUptime ::      Maybe String
  } deriving (Show, Eq)

data LeadKind = Normal | Debug | Non
    deriving (Show, Eq)

-- Everything one could ask for when debugging a failing script.
data Lead = Lead
  { leadKind ::        LeadKind
  , firstFailing ::    Maybe CallItem
  , hostInfo ::        HostInfo
  , interpreterInfo :: (Env, Log)
  , echoScript ::      Maybe Script
  } deriving (Show)

data Leadg = Leadg
  { gleadKind ::        LeadKind
  , gfirstFailing ::    Maybe CallItem
  , ghostInfo ::        HostInfo
  , ginterpreterInfo :: (Env, Log)
  , gechoScript ::      Maybe Scriptg
  } deriving (Show)

data InternalError = OutOfBounds
                   | Todo
    deriving (Show, Eq)

data HhError = LibError
             | SystemError
             | PointableError
    deriving (Show)


defaultScriptConfig :: ScriptConfig
defaultScriptConfig = ScriptConfig
  { retries = 0
  , maxDuration = Nothing
  , subjects = (Subject "a") Ls.:| []
  , useTls = Nothing
  }

effectiveTls :: Script -> Bool
effectiveTls Script { config = ScriptConfig { useTls = Just b } } = b
effectiveTls Script { callItems = (ci:_) } =
-- effectiveTls Script { configg = ScriptConfig { useTls = Just b } } = b
-- effectiveTls Script { callItemsg = (ci:_) } =
    -- case parseURI (lexedUrl (ciRequestSpec ci)) of
    case Nothing of
        Just uri | uriScheme uri == "http:" -> False
        _ -> True
effectiveTls _ = True -- Default to TLS if not specified

defaultDepsClause :: DepsClause
defaultDepsClause = DepsClause { deps = [], itemName = "" }

-- pCallItem :: DepsClause -> RequestSpec -> Maybe ResponseSpec -> CallItem
-- pCallItem dc rs opt =
--     CallItem
--       { ciDeps = deps dc
--       , ciName = itemName dc
--       , ciRequestSpec = rs
--       , ciResponseSpec = opt
--       }

gCallItem :: DepsClause -> RequestSpec -> Maybe ResponseSpec -> CallItem
gCallItem dc rs opt =
    CallItem
      { ciDeps = deps dc
      , ciName = itemName dc
      , ciRequestSpec = rs
      , ciResponseSpec = opt
      }

-- callItemIsDefault :: CallItem -> Bool
-- callItemIsDefault CallItem { ciName } = ciName == "default"

defaultHostInfo :: HostInfo
defaultHostInfo = HostInfo
  { hiHh200Conf = Nothing
  , hiHostname = Nothing
  , hiOs = Nothing
  , hiArch = Nothing
  , hiUptime = Nothing
  }

defaultLead :: Lead
defaultLead = Lead
    { leadKind = Normal
    , firstFailing = Nothing
    , hostInfo = defaultHostInfo
    , interpreterInfo = (HM.empty, [])
    , echoScript = Nothing
    }


showVerb :: UpperString -> String
showVerb (UpperString s) = s

-- ??:
showUrl :: UrlString -> String
showUrl (UrlString s) = s


noNews :: Leadg -> Bool
noNews (Leadg { gleadKind = Non }) = True
noNews _ = False

-- noNews :: Lead -> Bool
-- noNews (Lead { leadKind = Non }) = True
-- noNews _ = False

-- noNewsg :: Leadg -> Bool
-- noNewsg (Leadg { gleadKind = Non }) = True
-- noNewsg _ = False


-- -- Pretty-print.
present :: CallItem -> String
present cg =
    let rs = ciRequestSpec cg
    in method rs ++ " " ++ lexedUrl rs
       ++ (showHeaders $ headersg rs)
       ++ "\n" ++ (payloadg rs)
       ++ (showResponse $ ciResponseSpec cg) ++ "\n"

showHeaders :: RhsDict -> String
showHeaders (RhsDict hm) = concatMap fmt $ sortBy (compare `on` fst) $ HM.toList hm
    where
    fmt (k, v) = "\n" ++ k ++ ": " ++ unwords (map showPart v)

showPart :: BEL.Part -> String
showPart (BEL.R t) = Text.unpack t
showPart (BEL.L t) = Text.unpack t

showResponse :: Maybe ResponseSpec -> String
showResponse Nothing = ""
showResponse (Just rs) = "\nHTTP " ++ (unwords $ map (show . statusCode) (statuses rs))

data Dat = Dat [(Text, Text, Text)]
    deriving (Show)

--------------------------------------------------------------------------------
-- More lib than app code
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
