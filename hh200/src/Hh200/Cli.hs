module Hh200.Cli
    ( cli
    ) where

import System.IO (readFile)
import Control.Monad (foldM)
import Data.ByteString.Char8 (pack)
import Data.Version (showVersion)
import Options.Applicative

import Paths_hh200
-- import Hh200.Etf (wr)
import Hh200.Etf
import Hh200.Types
import L
-- import P ( Expr(..) )
import P

data Args = Args
    { source  :: Maybe String
    , version :: Bool
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


simpleRequest :: String
-- simpleRequest = "GET https://example.com\nHTTP 200\n"
-- simpleRequest = "GET https://example.com\n"
-- simpleRequest = "GET https://example.com"
-- simpleRequest = "HTTP 200\n"  -- ok
simpleRequest = "HTTP/1.1 200\n"  -- ok
-- simpleRequest = "HTTP/1 200\n"  -- ok

-- simpleRequest = "get https://example.com\n"  -- ok


go :: Args -> IO ()

-- hh200 --version
go (Args _ True) = putStrLn $ showVersion Paths_hh200.version

-- hh200 /home/tbmreza/gh/hh200/examples/hello.hhs
go (Args (Just s) _) = do
    let o = "output.etf"
    let tokensOrPanic = alexScanTokens simpleRequest
    let tokens = tokensOrPanic
    print tokens

    -- -- ??: send erlang
    -- program <- parseFile s
    -- print program
    --
    -- where
    --
    -- -- parseFile :: FilePath -> IO [Statement]
    -- -- parseFile filePath = do
    -- --     content <- readFile filePath
    -- --     let ast = parse $ alexScanTokens content :: E [Statement]
    -- --     case ast of
    -- --         Ok stmts -> return stmts
    -- --         Failed msg -> return []
    -- parseFile :: FilePath -> IO String
    -- parseFile filePath = do
    --     content <- readFile filePath
    --     return content
