module Hh200.Cli
    ( cli
    , interpret
    ) where

import System.IO (readFile)
import Control.Monad (foldM)
import Data.ByteString.Char8 (pack)
import Data.Version (showVersion)
import Options.Applicative

import GHC.Utils.Error
import qualified GHC.Utils.Outputable
import GHC.Driver.Ppr
import GHC.Data.FastString
import GHC.Types.SrcLoc

import Paths_hh200
import Hh200.Vm (vmRun, Vm)
import Hh200.Types
import L
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


go :: Args -> IO ()

-- hh200 --version
go (Args _ True) = putStrLn $ showVersion Paths_hh200.version

-- hh200 input.hhs
go (Args (Just s) _) = do
    program <- parseFile s
    print program

    interpret program where

    parseFile :: FilePath -> IO [Statement]
    parseFile filePath = do
        content <- readFile filePath
        let ast = parse $ alexScanTokens content  -- ??: handle (monadic?) fallible alexScanTokens "lexical error" https://github.com/haskell/alex/blob/7b5585326bed26265f924d4b87b5a1fe0456e4b8/data/AlexWrappers.hs#L471
        return ast

-- ??: assert response statusCode
--     withPolicy
--     handle panics: network/http, lexer,
--     reporting with spans: reuse GHC
--
-- Perform side effects (reporting or http) intended by program source.
-- Some of concurrency/parallelism semantics probably communicated by `interpret`'s signature.
interpret :: [Statement] -> IO ()
interpret program = do  -- program = [RequestLine{..}, Response{..}]
    -- type Vm = (HttpVerb, RequestHeaders, Url, RawString, ExpectCode, [Instr], Terminal)

    let vmInit = ("GET", [], "http://", "ignore...", 200, [], True)

    let loaded = foldr loadStatement vmInit program
    final <- vmRun loaded

    return () where

    loadStatement :: Statement -> Vm -> Vm
    loadStatement stmt acc = case (acc, stmt) of
        ((r1, r2, r3, r4, r5, r6, r7),
         RequestLine l1 (Url l21 l22 l23_ps l24_q l25_f)) -> (pack l1, [], r3 ++ l22 ++ concat l23_ps, r4, r5, HARDCODE:r6, False)

        ((r1, r2, r3, r4, r5, r6, r7),
         Response (IntLit t1)) ->                            (r1, r2, r3, r4, r5, ASSERT_CODE:r6, False)
