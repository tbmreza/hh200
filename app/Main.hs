{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.HashMap.Strict as HM
import qualified Data.HashTable.IO as H
import           Options.Applicative

-- import           Vm (vmRun, vmFrom, Vm, vmDefault, Instr (..))
import           Vm
-- import           Disk (load)
import           Disk

data Args = Args
    { hello      :: String
    , quiet      :: Bool
    , enthusiasm :: Int
    }

args :: Parser Args
args = Args
      <$> strOption
          ( long "hello"
         <> metavar "TARGET"
         <> help "Target for the greeting" )
      <*> switch
          ( long "quiet"
         <> short 'q'
         <> help "Whether to be quiet" )
      <*> option auto
          ( long "enthusiasm"
         <> help "Exclam count"
         <> showDefault
         <> value 1
         <> metavar "INT" )

main :: IO ()
main = cli =<< execParser opts
  where
    opts = info (args <**> helper)
      ( fullDesc
     <> header "Run hh200 scripts"
     )

cli :: Args -> IO ()
-- ??:
-- theoretical max size of Vm interpreting infinitely long .hhs script
-- logger
-- unit "http://httpbin.org/patch"
-- prop: methods only invoked by fit states

-- hh200 compile --io examples/hello.hhs [--specs specs.aimfor.toml]
cli (Args sourcePath False n) = do
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
cli (Args sourcePath True n) = do
    putStrLn "hh200 check examples/hello.hhs > out.hhir"

-- cli _ = return ()



emptyMap :: HM.HashMap String Int
emptyMap = HM.empty


type HttpCode = Int

type HttpVerb = String

-- type HashTable k v = H.BasicHashTable k v
-- type Headers = HashTable String String

type Terminal = Bool
-- type Url = String
