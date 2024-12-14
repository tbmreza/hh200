{-# Language OverloadedStrings #-}

module Disk where
-- module Disk (
--     load
--   , Instr
--   , overridePolicies
-- ) where

import           System.IO
import Types

import qualified Data.Attoparsec.ByteString as A (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as AC8
import Toml
-- import QuoteStr (quoteStr)
import Language.Haskell.TH (Exp(LitE), ExpQ, Lit(StringL))
import Language.Haskell.TH.Quote ( QuasiQuoter(..) )
import Data.List ( stripPrefix )
import Data.Text (Text)
import qualified Data.Text.IO as TIO

import System.Log.FastLogger
import System.Log.FastLogger.Date
import Control.Monad (forM_)
import Control.Concurrent (threadDelay)
import Data.Time.LocalTime (getZonedTime)

-- Function to set up and create a logger
-- ??: better lib https://github.com/freckle/blammo/tree/main/Blammo
setupLogger :: IO LoggerSet
setupLogger = do
    -- Create a logger that writes to a file with log rotation
    loggerSet <- newFileLoggerSet defaultBufSize "./logs/app.log"
    
    -- -- Optional: Add a date formatter
    -- dateTimeFormatter <- newDateFormatter "%Y-%m-%d %H:%M:%S"
    
    return loggerSet

type HhsmSource = FilePath
type CfgSource = FilePath  -- prop: "ends with .toml"


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
