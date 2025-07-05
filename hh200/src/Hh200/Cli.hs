module Hh200.Cli
    ( cli
    ) where

import System.Process (readProcess)
import System.Exit (ExitCode(..))
import Control.Exception (catch, SomeException)

import System.IO (readFile)
import Control.Monad (foldM)
import Data.ByteString.Char8 (pack)
import Data.Version (showVersion)
import Options.Applicative

import qualified Paths_hh200 (version)
import qualified Hh200.Types as Hh
import qualified Hh200.Fearless as Hh
import qualified Hh200.Scanner as Hh

data Args = Args
    { source  :: Maybe String
    , version :: Bool
    , call :: Bool
    }

cli :: IO ()
cli = go =<< execParser opts where
    opts = info (args <**> helper) (fullDesc
                                 <> header "Run hh200 scripts") where
    args = Args
        <$> optional (argument str (metavar "SOURCE"
                                 <> help "Path of source program"))
        <*> switch ( long "version"
                  <> short 'V'
                  <> help "Print version info and exit" )

        <*> switch ( long "call"
                  <> short 'C'
                  <> help "??" )

simpleRequest :: String
-- simpleRequest = "GET https://example.com\nHTTP 200\n"
-- simpleRequest = "GET https://example.com"
-- simpleRequest = "HTTP 200\n"  -- ok
simpleRequest = "HTTP/1.1 200\n"  -- ok
-- simpleRequest = "HTTP/1 200\n"  -- ok

-- simpleRequest = "get https://example.com\n"  -- ok

go :: Args -> IO ()

-- hh200 --version
go Args { version = True } = putStrLn $ showVersion Paths_hh200.version

-- hh200 --call
go Args { call = True } = do
    Hh.ratRace

-- hh200 /home/tbmreza/gh/hh200/examples/hello.hhs
go Args { source = Just src } = do
    Hh.runHttpM $ Hh.fromHhs src

go _ = return ()
