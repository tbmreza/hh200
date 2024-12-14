module Hh200.Cli where

import Options.Applicative
import Paths_hh200 as Lang (version)
import Data.Version (showVersion)

data Args = Args
    { source     :: Maybe String
    , quiet      :: Bool
    , version    :: Bool
    -- , enthusiasm :: Int
    }

args :: Parser Args
args = Args
      <$> optional (argument str (metavar "SOURCE" <> help "Path of source program"))
      <*> switch
          ( long "quiet"
         <> short 'q'
         <> help "Whether to be quiet" )
      <*> switch
          ( long "version"
         <> short 'V'
         <> help "Print version info and exit" )
      -- <*> option auto
      --     ( long "enthusiasm"
      --    <> help "Exclam count"
      --    <> showDefault
      --    <> value 1
      --    <> metavar "INT" )

-- main :: IO ()
-- main = 
--     let opts = info (helper <*> args)
--                (fullDesc <> header "Run hh200 scripts")
--     in
--     execParser opts >>= cli
--
-- ??: use this block to pass args to cli in golden
main' :: IO ()
main' = cli' =<< execParser opts
  where
    opts = info (args <**> helper)
      ( fullDesc
     <> header "Run hh200 scripts"
     )


-- clb :: String
-- clb = showVersion Lang.version

type StdOut = String
cli :: Args -> IO StdOut

cli (Args _ _ True) = do
    let v = showVersion Lang.version
    putStrLn v
    return v

cli _ = do
    putStrLn "todo"
    return ""

cli' :: Args -> IO ()
cli' _ = do return ()
--
-- cli (Args _ _ True) =
--     putStrLn $ showVersion Lang.version
--
-- cli _ = do
--     putStrLn "todo"
