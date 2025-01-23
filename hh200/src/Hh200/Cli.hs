module Hh200.Cli
    ( cli
    -- , 
    ) where

import System.IO (readFile)
import Data.Version (showVersion)
import Options.Applicative

import Paths_hh200
import Hh200.Vm (downloadFile)

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
    -- parseFile
    --
    -- FilePath ->        Ast -> IO ()
    --          parseFile     interpret
    -- httpClientCall "https://httpbin.org/anything"
    interp ()

type Ast = ()
-- Hardcode url to download here.
interp :: Ast -> IO ()
interp prog = do
    -- ... downloadFile "https://..." "out.apk"  -- ??: https tls
    -- () <- downloadFile "http://mobile-apps.paracorpgroup.com/nova/rc/nova%20v1.1.0-rc-sprint12.6.apk"
    () <- downloadFile "https://httpbin.org/image/png"
    
    return ()

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
