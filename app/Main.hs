{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.HashMap.Strict as HM
import qualified Data.HashTable.IO as H
import           Options.Applicative

-- import           Vm (vmRun, vmFrom, Vm, vmDefault, Instr (..))
import           Vm
import           Disk (load)

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

-- hh200 compile --io examples/hello.hhs [--specs specs.aimfor.toml]
cli (Args sourcePath False n) = do
    -- -- ok:
    -- instrs <- load sourcePath
    -- vm <- vmFrom instrs
    -- vmRun vm

    -- overridePolicies |> vmFrom |> vm compile (|> if !io writeToDisk else vmRun)
    --
    -- overridePolicies :: CfgSource -> IO Policy
    -- vmFrom :: Policy -> [Instr] -> IO Vm
    -- vmCompile :: IO Vm -> HhsmTree
    -- vmRun :: HhsmTree -> IO ()
    putStrLn "hereeeok"

-- hh200 interactive
cli (Args sourcePath True n) = do
    putStrLn "hh200 check examples/hello.hhs > out.hhir"

-- cli _ = return ()



emptyMap :: HM.HashMap String Int
emptyMap = HM.empty


type HttpCode = Int

type HashTable k v = H.BasicHashTable k v

type HttpVerb = String
type Headers = HashTable String String

hdrs :: IO Headers
hdrs = do
    tbl <- H.new
    return tbl

type Terminal = Bool
type Url = String
