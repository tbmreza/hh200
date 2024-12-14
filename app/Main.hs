-- ??:
-- theoretical max size of Vm interpreting infinitely long .hhs script
-- prop: methods only invoked by fit states
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Main where

import qualified Data.HashMap.Strict as HM
import qualified Data.HashTable.IO as H
import           Options.Applicative
import System.Environment (getArgs)

import           Vm
import           Disk
import Paths_hh200 as Project (version)
import Data.Version (showVersion)

data Args = Args
    { source     :: Maybe String
    , quiet      :: Bool
    , version    :: Bool
    , enthusiasm :: Int
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
      <*> option auto
          ( long "enthusiasm"
         <> help "Exclam count"
         <> showDefault
         <> value 1
         <> metavar "INT" )

-- main :: IO ()
-- main = cli =<< execParser opts
--   where
--     opts = info (args <**> helper)
--       ( fullDesc
--      <> header "Run hh200 scripts"
--      )
main :: IO ()
main = 
    let opts = info (helper <*> args)
               (fullDesc <> header "Run hh200 scripts")
    in
    execParser opts >>= cli


-- ??: import Hh200.Cli
cli :: Args -> IO ()

cli (Args _ _ True _) =
    putStrLn $ showVersion Project.version

-- hh200 compile --io examples/hello.hhs [--specs specs.aimfor.toml]
cli (Args _ False _ n) = do
    args <- getArgs
    let sourcePath = case args of
            [arg] -> arg
            _ -> ""  -- ??: putStrLn pwd or ls

    instrs <- parseHhs sourcePath
    policy <- overridePolicies sourcePath
    final <- vmRun $ vmFrom policy instrs

    -- overridePolicies |> vmFrom |> vmCompile (|> if !io writeToDisk else vmRun)
    --
    --              Hhsm -> Vm -> ()
    --            Policy ->
    --     
    -- Source -> [Instr] -> Vm -> ()
    --            Policy ->
    --
    -- Hhsm and [Instr] not quite aliases because .hhbc
    -- type Source = FilePath ending with .hhs
    -- type UserCfg = FilePath ending with .toml
    -- parseHhs :: Source -> IO [Instr]    overridePolicies :: CfgSource -> Policy
    -- vmFrom :: Policy -> [Instr] -> Vm
    -- vmRun :: IO Vm -> IO Vm
    return ()

-- hh200 interactive
cli (Args _ True _ n) = do
    putStrLn "hh200 check examples/hello.hhs > out.hhir"



type HttpCode = Int

type HttpVerb = String

type Terminal = Bool
