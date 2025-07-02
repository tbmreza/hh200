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
import Hh200.Types

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
-- simpleRequest = "GET https://example.com\n"
-- simpleRequest = "GET https://example.com"
-- simpleRequest = "HTTP 200\n"  -- ok
simpleRequest = "HTTP/1.1 200\n"  -- ok
-- simpleRequest = "HTTP/1 200\n"  -- ok

-- simpleRequest = "get https://example.com\n"  -- ok

chainCommands :: IO ()
chainCommands = do
    let cmd1 = "echo"
    let args1 = ["A=111,A."]  -- this invocation doesn't have access to running node's bindings
    let cmd2 = "erl_call"
    let args2 = ["-sname", "hh200", "-e"]

    -- Construct the full command with a pipe
    let fullCommand = cmd1 ++ " " ++ unwords args1 ++ " | " ++ cmd2 ++ " " ++ unwords args2

    res <- catch
        (readProcess "bash" ["-c", fullCommand] "")
        (\e -> return $ "[Hh200.Cli] " ++ show (e :: SomeException))
    putStrLn res


go :: Args -> IO ()

-- hh200 --version
go Args { version = True } = putStrLn $ showVersion Paths_hh200.version

-- hh200 --call
go Args { call = True } =
    chainCommands

go _ = return ()

-- -- hh200 /home/tbmreza/gh/hh200/examples/hello.hhs
