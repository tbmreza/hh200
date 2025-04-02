module Hh200.Cli
    ( cli
    -- , interpret
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
-- simpleRequest = "GET https://example.com\n"
simpleRequest = "HTTP 200"  -- ok
-- simpleRequest = "HTTP/1.1 200"  -- ok
-- simpleRequest = "HTTP/1 200"  -- ok

-- simpleRequest = "get https://example.com\n"  -- ok

-- asTerm :: [Statement] -> Term
asTerm _ = ListTerm [ TupleTerm [AtomTerm "post", asBinaryTerm "http://localhost:9999/413-Content-Too-Large.php"] , TupleTerm [AtomTerm "json", TupleTerm [AtomTerm "mut", asBinaryTerm "/home/tbmreza/gh/hh200/building-blocks/rt/asset.json", IntegerTerm 9]] , TupleTerm [AtomTerm "probe_valid_size", IntegerTerm 4100, IntegerTerm 200] ]

go :: Args -> IO ()

-- hh200 --version
go (Args _ True) = putStrLn $ showVersion Paths_hh200.version

-- hh200 /home/tbmreza/gh/hh200/examples/hello.hhs
go (Args (Just s) _) = do
    let o = "output.etf"
    let tokensOrPanic = alexScanTokens simpleRequest
    let tokens = tokensOrPanic
    print tokens

    case parse tokens of
        ParseOk a ->       writeTermToFile o $ asTerm a
        ParseFailed msg -> print msg

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


-- -- Perform side effects (reporting or http) intended by program source.
-- -- Some of concurrency/parallelism semantics probably communicated by `interpret`'s signature.
-- interpret :: [Statement] -> IO ()
-- interpret program = do  -- program = [RequestLine{..}, Response{..}]
--     -- mgr <- newManager tlsManagerSettings
--     let vmInit = ("GET", [], "http://", "ignore...", 200, [], True)
--
--     let loaded = foldr loadStatement vmInit program
--     final <- vmRun loaded
--
--     return () where
--
--     loadStatement :: Statement -> Vm -> Vm
--     loadStatement stmt acc = case (acc, stmt) of
--         ((r1, r2, r3, r4, r5, r6, r7),
--          RequestLine l1 (Url l21 l22 l23_ps l24_q l25_f)) -> (pack l1, [], r3 ++ l22 ++ concat l23_ps, r4, r5, X:r6, False)
--
--         ((r1, r2, r3, r4, r5, r6, r7),
--          Response (IntLit t1)) ->                            (r1, r2, r3, r4, r5, (MATCH_CODES t1):r6, False)
