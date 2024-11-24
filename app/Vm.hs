{-# LANGUAGE OverloadedStrings #-}

module Vm (
    Instr (..),
    Vm,
    step,
    vmDefault,
    vmFrom,
    vmRun
) where

import           Data.Aeson            (Value)
import qualified Data.HashMap.Strict   as HM
import qualified Data.ByteString       as S8
import qualified Data.ByteString.Char8 as C8
import qualified Data.Yaml             as Yaml
import           Network.HTTP.Simple

type HttpMethod = S8.ByteString  -- "GET" "POST"
type ExpectCode = Int
type HttpVerb = String
type Url = String
type Terminal = Bool
type Vm = (HttpVerb, Url, ExpectCode, [Instr], Terminal)

class State a where
    isTerminal :: a -> Bool
    topInstr :: IO a -> IO Instr
    popInstr :: IO a -> IO a
    setVerb :: IO a -> HttpVerb -> IO a
    setExpectCode :: IO a -> ExpectCode -> IO a

instance State Vm where
    isTerminal (_, _, _, _, b) = b
    setVerb ioVm v = do
        (_, e1, e2, e3, e4) <- ioVm
        return (v, e1, e2, e3, e4)
    setExpectCode ioVm c = do
        (e0, e1, _, e3, e4) <- ioVm
        return (e0, e1, c, e3, e4)
    topInstr ioVm = do
        (_, _, _, instrs, _) <- ioVm
        return $ head instrs
    popInstr ioVm = do
        (e0, e1, e2, instrs, e4) <- ioVm
        return (e0, e1, e2, tail instrs, e4)

data Instr
    = NOP
    | IV
    | IC
    | IH
    | OV String

vmDefault :: IO Vm
vmDefault = do return ("get", "", 200, [], False)

vmFrom :: [Instr] -> IO Vm
vmFrom hhirInstrs = do return ("get", "", 200, hhirInstrs, False)

vmVerb :: Vm -> HttpVerb
vmVerb (i, _, _, _, _) = i
vmUrl :: Vm -> Url
vmUrl (_, j, _, _, _) = j
vmExpect :: Vm -> ExpectCode
vmExpect (_, _, k, _, _) = k
vmInstrs :: Vm -> [Instr]
vmInstrs (_, _, _, l, _) = l
vmTerminal :: Vm -> Terminal
vmTerminal (_, _, _, _, m) = m

-- -- Instr: override verb
-- overrideVerb :: IO Vm -> HttpVerb -> IO Vm
-- overrideVerb ioVm x = do
--     vm <- ioVm
--     return (x, vmUrl vm, vmExpect vm, vmInstrs vm, vmTerminal vm)

-- -- Instr: override expect_code
-- overrideExpectCode :: IO Vm -> ExpectCode -> IO Vm
-- overrideExpectCode ioVm x = do
--     vm <- ioVm
--     return (vmVerb vm, vmUrl vm, x, vmInstrs vm, vmTerminal vm)

-- Progress by executing top Instr. IO is necessary because HTTP calls can be performed when stepping.
step :: IO Vm -> IO Vm
step ioVm = do
    instr <- topInstr ioVm
    go instr where
        go IV = setVerb ioVm "get"
        go IC = setExpectCode ioVm 200
        go NOP = popInstr ioVm
        go _ = vmDefault

    -- unused <- ioPost HM.empty (method (vmVerb vm)) "http://httpbin.org/patch"
    -- unused <- ioPost HM.empty (method (vmVerb vm)) (vmUrl vm)

step _ = vmDefault

method :: String -> S8.ByteString
method s = C8.pack s

type Vars = HM.HashMap String String
ioPost :: Vars -> HttpMethod -> Url -> IO Int
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

-- Virtual machine is final if previous state set it to be Terminal
-- or if there's no [Instr] left.
isFinal :: Vm -> Bool
isFinal (_, _, _, _, True) = True
isFinal (_, _, _, [], _) = True
isFinal _ = False

vmRun :: Vm -> IO ()
vmRun vm = do
    -- until isFinal step
    putStrLn "todo run"
