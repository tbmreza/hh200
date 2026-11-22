{-# LANGUAGE OverloadedStrings #-}

-- | What a run takes from outside the script: external variables
-- (@--var NAME=VALUE@ and @HH200_VAR_NAME@ environment variables) and the
-- policy for files a script reads through @file,@ and @base64-file,@ values
-- in a @[Vars]@ block.
module Hh200.Inputs
    ( RunInputs (..)
    , defaultRunInputs
    , FilePolicy (..)
    , defaultFilePolicy
    , ScriptInputError (..)
    , varEnvPrefix
    , externalVars
    , parseVarArg
    , seedEnv
    , confinedPath
    , resolveScriptFile
    , readVarValue
    ) where

import           Control.Exception (Exception)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import           Data.Char (isAlphaNum, isDigit)
import qualified Data.HashMap.Strict as HM
import           Data.List (isPrefixOf, stripPrefix)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import           System.Directory (canonicalizePath, doesFileExist, makeAbsolute)
import           System.FilePath ((</>), isRelative, normalise, splitDirectories)

import qualified BEL
import           Hh200.Types (Env, newEnv)

-- | Where files named by a script are looked up.
data FilePolicy = FilePolicy
    { fpBaseDir :: FilePath
      -- ^ Relative paths resolve against this: the script's directory for a
      -- script file, the working directory for @--call@ snippets.
    , fpRoot    :: Maybe FilePath
      -- ^ When set, every read must stay under this directory. Dashboard runs
      -- set it to @HH200_SCRIPT_ROOT@ so a submitted script can't read (and
      -- then send) arbitrary files from the server.
    } deriving (Show, Eq)

defaultFilePolicy :: FilePolicy
defaultFilePolicy = FilePolicy { fpBaseDir = ".", fpRoot = Nothing }

-- | Everything a run takes from outside the script.
data RunInputs = RunInputs
    { riVars  :: HM.HashMap Text Aeson.Value  -- ^ external variables
    , riFiles :: FilePolicy
    } deriving (Show, Eq)

defaultRunInputs :: RunInputs
defaultRunInputs = RunInputs { riVars = HM.empty, riFiles = defaultFilePolicy }

-- | A call item can't be sent because of its inputs: an unbound @{{NAME}}@, a
-- missing file, or a path outside the script root. Raised before any request
-- for that call item goes out.
newtype ScriptInputError = ScriptInputError String
    deriving (Show)

instance Exception ScriptInputError

-- | Environment variables with this prefix become script variables, minus the
-- prefix (@HH200_VAR_API_KEY@ binds @API_KEY@). Only prefixed variables are
-- exposed, so the dashboard doesn't hand its whole environment to scripts.
varEnvPrefix :: String
varEnvPrefix = "HH200_VAR_"

-- | External variables from the process environment and @--var@ pairs. A
-- @--var@ wins over an @HH200_VAR_*@ variable of the same name.
externalVars :: [(String, String)] -> [(String, String)] -> HM.HashMap Text Aeson.Value
externalVars environment cliVars = HM.fromList (fromEnv ++ fromCli)
  where
    fromEnv = [ (Text.pack name, Aeson.String (Text.pack v))
              | (k, v) <- environment
              , Just name <- [stripPrefix varEnvPrefix k]
              , validName name ]
    fromCli = [ (Text.pack k, Aeson.String (Text.pack v)) | (k, v) <- cliVars ]

-- | Parse a @--var@ argument, @NAME=VALUE@. The value may contain @=@.
parseVarArg :: String -> Either String (String, String)
parseVarArg s = case break (== '=') s of
    (name, '=' : value) | validName name -> Right (name, value)
    _ -> Left ("--var expects NAME=VALUE with NAME made of letters, digits and _, got: " ++ s)

validName :: String -> Bool
validName name@(c : _) = not (isDigit c) && all (\x -> isAlphaNum x || x == '_') name
validName [] = False

-- | The environment a run starts from: the external variables as bindings.
seedEnv :: RunInputs -> Env
seedEnv inputs = newEnv { BEL.bindings = riVars inputs }

-- | Resolve @raw@ against @base@ (when relative) to a canonical absolute path.
-- With a root, return 'Nothing' unless the result lies strictly under it.
confinedPath :: Maybe FilePath -> FilePath -> FilePath -> IO (Maybe FilePath)
confinedPath mRoot base raw = do
    let joined = if isRelative raw then base </> raw else raw
    candAbs <- makeAbsolute joined >>= canonicalizePath
    case mRoot of
        Nothing -> pure (Just candAbs)
        Just root -> do
            rootAbs <- makeAbsolute root >>= canonicalizePath
            let rootDirs = splitDirectories (normalise rootAbs)
                candDirs = splitDirectories (normalise candAbs)
            pure $ if rootDirs `isPrefixOf` candDirs && length candDirs > length rootDirs
                then Just candAbs
                else Nothing

-- | Resolve a file a script names under the policy, and check it exists.
resolveScriptFile :: FilePolicy -> FilePath -> IO (Either String FilePath)
resolveScriptFile policy raw = do
    mPath <- confinedPath (fpRoot policy) (fpBaseDir policy) raw
    case mPath of
        Nothing -> pure (Left ("file path escapes the script root: " ++ raw))
        Just path -> do
            exists <- doesFileExist path
            pure $ if exists
                then Right path
                else Left ("file not found: " ++ raw ++ " (resolved to " ++ path ++ ")")

-- | The value of a rendered @[Vars]@ entry. @file,PATH@ binds the file's text
-- (UTF-8); @base64-file,PATH@ binds the base64 of its bytes; anything else is
-- bound as the string itself.
readVarValue :: FilePolicy -> Text -> IO (Either String Aeson.Value)
readVarValue policy t
    | Just p <- Text.stripPrefix "base64-file," t = withFile p $ \bytes ->
        Right (Aeson.String (TE.decodeLatin1 (B64.encode bytes)))
    | Just p <- Text.stripPrefix "file," t = withFile p $ \bytes ->
        case TE.decodeUtf8' bytes of
            Right text -> Right (Aeson.String text)
            Left _ -> Left ("file is not valid UTF-8 text: " ++ Text.unpack (Text.strip p)
                            ++ " (use base64-file, for binary files)")
    | otherwise = pure (Right (Aeson.String t))
  where
    withFile p k = do
        resolved <- resolveScriptFile policy (Text.unpack (Text.strip p))
        case resolved of
            Left err -> pure (Left err)
            Right path -> k <$> BS.readFile path
