{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hh200.Types
    ( UpperString (..)
    , Snippet (..)
    , RhsDict (..), mtRhsDict
    , mtHM
    , Duration
    , Subject (..)
    , Binding
    , Env
    , TraceEvent (..)
    , Log
    , ScriptKind (..)
    , Script (..)
    , ScriptConfig (..)
    , DepsClause (..)
    , CallItem (..)
    , RequestSpec (..)
    , ResponseSpec (..)
    , HostInfo (..)
    , LeadKind (..)
    , Lead (..)
    , InternalError (..)
    , HhError (..)
    , defaultScriptConfig
    , defaultDepsClause
    , defaultHostInfo
    , show'
    , trimQuotes
    , expectUpper, expectUrl
    , showVerb
    , noNews
    , present
    , showHeaders
    , showPart
    , showResponse
    , Dat (..)
    , newEnv
    , LexedUrl (..)
    , ResponseSquare (..), rpSquaresNothing
    , RequestSquare (..), rqSquaresNothing

    -- temp
    , mkScript
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

newtype UrlString = UrlString String
    deriving (Show, Eq)

expectUrl :: String -> UrlString
expectUrl s = UrlString s

newtype Snippet = Snippet L8.ByteString

-- Cookie is a native concept in BEL.
newtype RhsDict = RhsDict (HM.HashMap Text [BEL.Part])
    deriving (Show, Eq)

mtRhsDict = RhsDict HM.empty

mtHM :: HM.HashMap k v
mtHM = HM.empty

type Duration = Int

newtype Subject = Subject String
    deriving (Show, Eq)

type Binding = (Text, [BEL.Part])

type Env = BEL.Env -- = HM.HashMap String Aeson.Value
newEnv :: Env
newEnv = BEL.Env { BEL.bindings = HM.empty }

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

data Script = Script
  { kind :: ScriptKind
  , config :: ScriptConfig
  , callItems :: [CallItem]
  } deriving (Show)

mkScript :: Script
mkScript = Script
  { kind = Regular
  , config = ScriptConfig { }
  -- , callItems = [ ciw ]
  }

ciw :: CallItem
ciw = CallItem
  { ciDeps = []
  , ciName = "default"
  , ciRequestSpec = RequestSpec
    { rqMethod = "GET"
    -- , rqUrl = "http://localhost:80"
    , rqUrl = LexedUrlFull "http://localhost:9999/echo"
    , rqHeaders = RhsDict HM.empty
    -- , rqConfigs = RhsDict HM.empty
    , rqBody = ""
    }
  , ciResponseSpec = Nothing
  }

data ScriptConfig = ScriptConfig
  { retries :: Int
  , maxDuration :: Maybe Duration
  , subjects :: Ls.NonEmpty Subject
  -- , respectUrlInsecureHttp :: Maybe Bool  -- ??: ScriptConfig definition and its derivatives mean very little until some of them are used in one of Execution modes.
  , useTls :: Maybe Bool
  } deriving (Show, Eq)

data DepsClause = DepsClause
  { deps :: [String]
  , itemName :: String
  }

data CallItem = CallItem
  { ciDeps ::         [String]
  , ciName ::         String
  , ciRequestSpec ::  RequestSpec
  , ciResponseSpec :: Maybe ResponseSpec
  } deriving (Show)

-- uri needs eval only if it contains matching {{}}
--
-- designing a 
-- when building Request will definitely fail:
-- uneven {{}}; (unallowed/escaped??) chars.
--
-- same trip: Special-Use Domain Names like localhost

data LexedUrl =
    LexedUrlFull String
  | LexedUrlSegments [BEL.Part]
    deriving (Show)

data RequestSquare =
    RequestSquareConfigs RhsDict
  | RequestSquareQuery RhsDict
  | RequestSquareForm RhsDict
  | RequestSquareMultipart RhsDict
  | RequestSquareCookies RhsDict
    deriving (Show)

data ResponseSquare =
    ResponseSquareAsserts [String]
  | ResponseSquareCaptures RhsDict
    deriving (Show)

data RequestSpec = RequestSpec
  { rqMethod ::  String
  -- Static array of hh200 square blocks: Configs, Query, Form, MultipartFormData, Cookies
  , rqSquares :: (Maybe RequestSquare, Maybe RequestSquare, Maybe RequestSquare, Maybe RequestSquare, Maybe RequestSquare)
  , rqUrl ::     LexedUrl
  , rqHeaders :: RhsDict
  , rqBody ::    String
  } deriving (Show)

rqSquaresNothing :: (Maybe RequestSquare, Maybe RequestSquare, Maybe RequestSquare, Maybe RequestSquare, Maybe RequestSquare)
rqSquaresNothing = (Nothing, Nothing, Nothing, Nothing, Nothing)

rpSquaresNothing :: (Maybe ResponseSquare, Maybe ResponseSquare)
rpSquaresNothing = (Nothing, Nothing)

data ResponseSpec = ResponseSpec
  { rpStatuses :: [Status]

  -- Static array of hh200 square blocks: Captures, Asserts, Output
  -- [Asserts] List of untyped expr line, input for evaluator.
  , rpSquares :: (Maybe ResponseSquare, Maybe ResponseSquare)

  -- Response and representation headers.
  , rpResponseHeaders :: RhsDict
  , rpBody :: String
  }
  -- deriving (Show, Eq)
  deriving (Show)

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
  }

mkLead :: Lead
mkLead = Lead
  { leadKind = Non
  , firstFailing = Nothing
  , hostInfo = defaultHostInfo
  -- , interpreterInfo = (HM.empty, [])
  , echoScript = Nothing
  }

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

defaultDepsClause :: DepsClause
defaultDepsClause = DepsClause { deps = [], itemName = "" }

defaultHostInfo :: HostInfo
defaultHostInfo = HostInfo
  { hiHh200Conf = Nothing
  , hiHostname = Nothing
  , hiOs = Nothing
  , hiArch = Nothing
  , hiUptime = Nothing
  }

showVerb :: UpperString -> String
showVerb (UpperString s) = s

noNews :: Lead -> Bool
noNews (Lead { leadKind = Non }) = True
noNews _ = False

-- Pretty-print.
present :: CallItem -> String
present cg =
    let rs = ciRequestSpec cg
    -- in rqMethod rs ++ " " ++ lexedUrl rs  -- ??
    in rqMethod rs ++ " "
       ++ (showHeaders $ rqHeaders rs)
       ++ "\n" ++ (rqBody rs)
       ++ (showResponse $ ciResponseSpec cg) ++ "\n"

showHeaders :: RhsDict -> String
showHeaders (RhsDict hm) = concatMap fmt $ sortBy (compare `on` fst) $ HM.toList hm
    where
    fmt (k, v) = Text.unpack $ Text.cons '\n' k <> Text.pack (": " ++ unwords (map showPart v))

showPart :: BEL.Part -> String
showPart (BEL.R t) = Text.unpack t
showPart (BEL.L t) = Text.unpack t

showResponse :: Maybe ResponseSpec -> String
showResponse Nothing = ""
showResponse (Just rs) = "\nHTTP " ++ (unwords $ map (show . statusCode) (rpStatuses rs))
    ++ (showHeaders $ rpResponseHeaders rs)

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
