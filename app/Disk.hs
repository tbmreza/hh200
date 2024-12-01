{-# Language OverloadedStrings #-}

module Disk (
    load
  , Instr
  , overridePolicies
) where

-- import           Data.Aeson            (Value)
-- import qualified Data.ByteString       as S8
-- import qualified Data.ByteString.Char8 as C8
-- import qualified Data.Yaml             as Yaml
-- import           System.IO (Handle, hIsEOF, withFile, IOMode(ReadMode), hGetLine)
import           System.IO
-- import Types (Instr (..))
import Types

import qualified Data.Attoparsec.ByteString as A (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as AC8
-- import Toml (decode)
import Toml
-- import QuoteStr (quoteStr)
import Language.Haskell.TH (Exp(LitE), ExpQ, Lit(StringL))
import Language.Haskell.TH.Quote ( QuasiQuoter(..) )
import Data.List ( stripPrefix )
import Data.Text (Text)
import qualified Data.Text.IO as TIO

-- -- unit
-- -- hhsmParse :: FilePath -> [Instr]
-- -- vmFrom $ hhsmParse tmp.hhsm
--
type HhsmSource = FilePath
type CfgSource = FilePath  -- prop: "ends with .toml"
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

quoteStr :: QuasiQuoter
quoteStr = QuasiQuoter {
    quoteDec = \_ -> fail "quoteStr doesn't support declarations",
    quotePat = \_ -> fail "quoteStr doesn't support patterns",
    quoteType = \_ -> fail "quoteStr doesn't support types",
    quoteExp = processString
  }

processString :: String -> ExpQ
processString ('\n':xs) =
    let ws = takeWhile (' '==) xs

        cleanup "" = pure ""
        cleanup x = case stripPrefix ws x of
                      Nothing -> fail "bad prefix"
                      Just x' -> pure x'
    in LitE . StringL . unlines <$> traverse cleanup (lines xs)
processString _ = fail "malformed string literal"

readAllContents :: FilePath -> IO Text
readAllContents p = TIO.readFile p

overridePolicies :: CfgSource -> IO Policy
overridePolicies p = do
    src <- readAllContents p
    let res = decode src :: Result String Policy
    case res of
        Failure _ -> return policyDefault
        Success _ userP -> return $ policyOrDefault userP

-- decode [quoteStr|
--             stride = 2
--             all_stats = "forget"|]
--
-- decode [quoteStr|
--             max_reruns = 2
--             max_retries_per_call = 2
--             time_millis_per_call = 60_000|]
--
