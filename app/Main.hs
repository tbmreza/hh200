{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson            (Value)
import qualified Data.ByteString       as S8
import qualified Data.ByteString.Char8 as C8
import qualified Data.Yaml             as Yaml
import           Network.HTTP.Simple
import qualified Data.Attoparsec.ByteString as A (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as AC8
import qualified Data.HashMap.Strict as HM
import qualified Data.HashTable.IO as H
import           Options.Applicative

main :: IO ()
main = do
    zz <- step $ step initial
    putStrLn $ "todo"


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
type HttpMethod = S8.ByteString  -- GET POST

type Vars = HM.HashMap String String

method :: String -> S8.ByteString
method s = C8.pack s

ioPost :: Vars -> HttpMethod -> Url -> IO HttpCode
ioPost _ methd url = do
    -- request' <- parseRequest (methd ++ url)
    request' <- parseRequest url
    let request
            -- = setRequestPort 443
            -- = setRequestPort 8787
            = setRequestMethod methd
            -- = setRequestMethod "POST"
            -- $ setRequestPath "/put"
            $ setRequestQueryString [("hello", Just "world")]
            $ setRequestBodyLBS "This is my request body"
            -- $ setRequestSecure True
            $ request'
    response <- httpJSON request

    putStrLn $ "The status code was: " ++
               show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    C8.putStrLn $ Yaml.encode (getResponseBody response :: Value)

    return 200

type HashTable k v = H.BasicHashTable k v

type HttpVerb = String
type Instr = String
type Headers = HashTable String String

-- data Vm =
--     Vm {
--         verb :: HttpVerb,
--         prog :: [Instr]
--         -- headers :: Headers
--     }
--     deriving (Show)

hdrs :: IO Headers
hdrs = do
    tbl <- H.new
    return tbl

-- init :: HttpVerb -> IO Vm
-- init _ = do
--     return $ Vm "v"
--     -- return $ Vm "v" hdrs

-- init :: HttpVerb -> IO Headers
-- init _ = do
--     tbl <- H.new
--     return tbl

-- type Vm = (HttpVerb, [Instr])
type Terminal = Bool
type Url = String
type Vm = (HttpVerb, Url, [Instr], Terminal)
initial :: IO Vm
initial = do return ("get", "", [], False)

-- Virtual machine is final if previous state set it to be Terminal
-- or if there's no [Instr] left.
isFinal :: Vm -> Bool
isFinal (_, _, _, True) = True
isFinal (_, _, [], _) = True
isFinal _ = False

vmVerb :: Vm -> HttpVerb
vmVerb (i, _, _, _) = i
vmUrl :: Vm -> Url
vmUrl (_, j, _, _) = j

-- Progress by executing top Instr.
step :: IO Vm -> IO Vm
-- step IO (verb, parametrized_url, hhir_instrs, False) = initial
step ioVm = do
    vm <- ioVm
    let verb = vmVerb vm
    let url = vmUrl vm
    -- if instr == "x", verb == "post":
    unused <- ioPost HM.empty (method "PATCH") "http://httpbin.org/patch"
    return ("get", "", [], False)

step _ = initial

-- interpret :: 
-- interpret =
--     until isFinal step
