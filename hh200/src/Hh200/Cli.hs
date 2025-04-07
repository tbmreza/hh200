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
import Hh200.Etf
import Hh200.Types
import L
-- import P

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

erlRepl :: [String] -> String -> IO ()
erlRepl args replInput = do
    res <- catch
        (readProcess "erl" args replInput)
        (\e -> return $ "[Hh200.Cli] " ++ show (e :: SomeException))
    putStrLn res

erlCall :: [String] -> IO ()
erlCall args = do
    let replInputNone = ""
    res <- catch
        (readProcess "erl_call" args replInputNone)
        (\e -> return $ "[Hh200.Cli] " ++ show (e :: SomeException))
    putStrLn res

erlEval :: [String] -> IO ()
erlEval words = do
    let replInputNone = ""
    res <- catch
        (readProcess "echo" words replInputNone)
        (\e -> return $ "[Hh200.Cli] " ++ show (e :: SomeException))
    putStrLn res

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
-- $ erl_call -sname hh200 -a 'init stop'
go Args { call = True } =
    -- erlCall ["-sname", "hh200", "-a", "init stop"]
    -- erlCall ["-sname", "hh200", "-setcookie", "ZTDCNMUDFQEYFLUVJUAT"]
    -- erlCall ["-sname", "hh200"]
    -- erlRepl ["-sname", "hh200"] "{."

    -- -- rt:dbg() to an already running node
    -- erlCall ["-sname", "hh200", "-a", "rt dbg"]

    -- erlCall ["-sname", "hh200", "-e", "X=1,Y=2,{X,Y}"]
    -- erlCall ["-sname", "hh200", "-e", "X=1,Y=2,{X,Y}."]
    -- echo "X=1,Y=2,{X,Y}." | erl_call -sname hh200 -e
    -- erlEval ["X=1,Y=2,{X,Y}.", "|", "erl_call", "-sname", "hh200", "-e"]

    chainCommands

-- -- hh200 /home/tbmreza/gh/hh200/examples/hello.hhs
-- go (Args (Just s) _) = do
--     let o = "output.etf"
--     let tokensOrPanic = alexScanTokens simpleRequest
--     let tokens = tokensOrPanic
--     print tokens
--
--     -- -- ??: send erlang
--     -- program <- parseFile s
--     -- print program
--     --
--     -- where
--     --
--     -- -- parseFile :: FilePath -> IO [Statement]
--     -- -- parseFile filePath = do
--     -- --     content <- readFile filePath
--     -- --     let ast = parse $ alexScanTokens content :: E [Statement]
--     -- --     case ast of
--     -- --         Ok stmts -> return stmts
--     -- --         Failed msg -> return []
--     -- parseFile :: FilePath -> IO String
--     -- parseFile filePath = do
--     --     content <- readFile filePath
--     --     return content
