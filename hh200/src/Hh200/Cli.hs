module Hh200.Cli
    ( cli
    ) where

import System.IO (readFile)
import Control.Monad (foldM)
import Data.ByteString.Char8 (pack)
import Data.Version (showVersion)
import Options.Applicative

import Paths_hh200
import Hh200.Vm (downloadFile, vmRun, Vm)
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

    interpret program

interpret :: [Statement] -> IO ()
interpret program = do  -- program = [RequestLine{..}]
    -- Doesn't throw on invalid source.
    let start = vmFrom program  -- ??: and Policy

    vmRun start -- ??: remove
    final <- foldM processStatement start program
    return ()
    where  -- ??: statement is a tree that can have unevaled expressions in its leaf
        processStatement :: Vm -> Statement -> IO Vm
        processStatement acc stmt = do
            return ("GET", [], "", "", 200, [], False)


vmFrom :: [Statement] -> Vm

vmFrom [RequestLine method (Url s1 s2 paths q f)] = (pack method, [], "http://" ++ s2 ++ concat paths, "ignore...", 200, [HARDCODE], False)

vmFrom _ = ("GET", [], "http://localhost:9999/else.php", "ignore...", 200, [X], False)


-- type Ast = ()
-- -- Hardcode url to download here.
-- interp :: Ast -> IO ()
-- interp prog = do
--     -- let tokens = lexer "POST https://httpbin.org/image/png"  -- ok
--     -- ... downloadFile "https://..." "out.apk"  -- ??: https tls
--     -- () <- downloadFile "http://mobile-apps.paracorpgroup.com/nova/rc/nova%20v1.1.0-rc-sprint12.6.apk"
--     () <- downloadFile "https://httpbin.org/image/png"
--     
--     return ()

parseFile :: FilePath -> IO [Statement]
parseFile filePath = do
    content <- readFile filePath
    let ast = parse $ alexScanTokens content  -- ??: handle (monadic?) fallible alexScanTokens "lexical error" https://github.com/haskell/alex/blob/7b5585326bed26265f924d4b87b5a1fe0456e4b8/data/AlexWrappers.hs#L471
    return ast

-- parseFile :: FilePath -> IO (Either ParseError Ast)
-- parseFile :: FilePath -> IO (Either ParseError Expr)
-- parseFile filePath = do
--     content <- readFile filePath
--     return $ parse exprParser filePath content


-- import Options.Applicative
-- import Paths_hh200 as Lang (version)
-- import Data.Version (showVersion)
--
-- data Args = Args
--     { source     :: Maybe String
--     , quiet      :: Bool
--     , version    :: Bool
--     -- , enthusiasm :: Int
--     }
--
-- args :: Parser Args
-- args = Args
--       <$> optional (argument str (metavar "SOURCE" <> help "Path of source program"))
--       <*> switch
--           ( long "quiet"
--          <> short 'q'
--          <> help "Whether to be quiet" )
--       <*> switch
--           ( long "version"
--          <> short 'V'
--          <> help "Print version info and exit" )
--       -- <*> option auto
--       --     ( long "enthusiasm"
--       --    <> help "Exclam count"
--       --    <> showDefault
--       --    <> value 1
--       --    <> metavar "INT" )
--
-- -- main :: IO ()
-- -- main = 
-- --     let opts = info (helper <*> args)
-- --                (fullDesc <> header "Run hh200 scripts")
-- --     in
-- --     execParser opts >>= cli
-- --
-- main' :: IO ()
-- main' = cli' =<< execParser opts
--   where
--     opts = info (args <**> helper)
--       ( fullDesc
--      <> header "Run hh200 scripts"
--      )
--
--
-- -- clb :: String
-- -- clb = showVersion Lang.version
--
-- type StdOut = String
-- cli :: Args -> IO StdOut
--
-- cli (Args _ _ True) = do
--     let v = showVersion Lang.version
--     putStrLn v
--     return v
--
-- cli _ = do
--     putStrLn "todo"
--     return ""
--
-- cli' :: Args -> IO ()
-- cli' _ = do return ()
-- --
-- -- cli (Args _ _ True) =
-- --     putStrLn $ showVersion Lang.version
-- --
-- -- cli _ = do
-- --     putStrLn "todo"
