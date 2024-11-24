{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson            (Value)
import qualified Data.ByteString       as S8
import qualified Data.ByteString.Char8 as C8
import qualified Data.Yaml             as Yaml
import qualified Data.Attoparsec.ByteString as A (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as AC8
import qualified Data.HashMap.Strict as HM
import qualified Data.HashTable.IO as H
import           Options.Applicative
import           System.IO (Handle, hIsEOF, withFile, IOMode(ReadMode), hGetLine)

import           Vm (vmRun, vmFrom, Vm, vmDefault, Instr (..))

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

-- module Disk
-- -- unit
-- -- hhsmParse :: FilePath -> [Instr]
-- -- vmFrom $ hhsmParse tmp.hhsm
--
type HhsmSource = String
--
-- -- -- load "examples/hhsm/hello.hhsm"


load :: HhsmSource -> IO [Instr]
load path = withFile path ReadMode $ \handle -> do
    processLines handle []

parseLn :: String -> IO Instr
parseLn line = do return NOP

processLines :: Handle -> [Instr] -> IO [Instr]
processLines handle acc = do
    eof <- hIsEOF handle
    if eof
        then return (reverse acc)
        else do
            line <- hGetLine handle
            parsed <- parseLn line
            processLines handle (parsed : acc)

            -- return $ parseLn line


-- processLines :: Handle -> IO ()
-- processLines handle = do
--     eof <- hIsEOF handle
--     if eof
--         then return ()
--         else do
--             line <- hGetLine handle
--             parseLn line
--             processLines handle
--
-- -- parseLn "IV\n"
-- parseLn :: String -> IO Instr
-- parseLn line = do
--  return NOP



-- module Disk

cli :: Args -> IO ()
-- ??:
-- sourcePath as main position arg
-- theoretical max size of Vm interpreting infinitely long .hhs script
-- warn if file path doesn't end with .hhir

-- hh200 run out.hhir
cli (Args sourcePath False n) = do
    instrs <- load sourcePath
    vm <- vmFrom instrs
    vmRun vm
    -- putStrLn $ "Hellokkk, " ++ sourcePath ++ replicate n '!'

-- hh200 check examples/hello.hhs > out.hhir
cli (Args sourcePath True n) = do
    putStrLn "hh200 check examples/hello.hhs > out.hhir"

cli _ = return ()



isWs :: Char -> Bool
isWs ' ' = True
isWs ',' = True
isWs '\n' = True
isWs '\r' = True
isWs '\t' = True
isWs _ = False

ws :: A.Parser ()
ws = AC8.skipWhile isWs

comment :: A.Parser ()
comment =
    ws >> AC8.char '#' >> AC8.skipWhile (/= '\n')


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
