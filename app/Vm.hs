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
-- import Types (Instr (..))
import Types

type HttpMethod = S8.ByteString  -- "get" "post"
type ExpectCode = Int
type Url = String
type Terminal = Bool
type Vm = (HttpVerb, Url, ExpectCode, [Instr], Terminal)

class VmT a where
    setTerminal :: IO a -> IO a
    peekInstr :: IO a -> IO Instr
    popInstr :: IO a -> IO a
    setVerb :: IO a -> HttpVerb -> IO a
    setExpectCode :: IO a -> ExpectCode -> IO a
    executeVerb :: IO a -> IO a
    httpClientCall :: IO a -> IO (CodesMatch, a)

instance VmT Vm where
    setTerminal ioVm = do
        (e0, e1, e2, e3, e4) <- ioVm
        return (e0, e1, e2, e3, True)

    setVerb ioVm v = do
        (_, e1, e2, e3, e4) <- ioVm
        return (v, e1, e2, e3, e4)
    setExpectCode ioVm c = do
        (e0, e1, _, e3, e4) <- ioVm
        return (e0, e1, c, e3, e4)
    peekInstr ioVm = do
        (_, _, _, instrs, _) <- ioVm
        return $ head instrs

    popInstr ioVm = do
        (e0, e1, e2, instrs, e4) <- ioVm
        return (e0, e1, e2, tail instrs, e4)

    -- ??: httpClientCall type signature may communicate the Vm instances that are
    -- interested in invoking HTTP calls.
    --
    -- (_, _, _, X : rest, False)
    httpClientCall ioVm = do
        (verb, url, expectCode, instrs, terminal) <- ioVm
        let invalidState = False  -- ??: can wait until integration tests
        if invalidState
            then do return $ (False, (verb, url, expectCode, tail instrs, True))
            else do
                baseRequest <- parseRequest url
                let request = setRequestMethod verb
                            $ setRequestQueryString [("key", Just "val")]
                            $ setRequestBodyLBS "todo"
                            $ baseRequest
                resp <- httpJSON request

                let respBody = getResponseBody resp :: Value

                next <- popInstr ioVm
                return $ (getResponseStatusCode resp == expectCode, next)

    -- Execute machine's state.
    executeVerb ioVm = do
        -- Whether state's ExpectCode matches actual response's status code.
        (codesMatch, next) <- httpClientCall ioVm
        case codesMatch of
            True -> return next
            False -> setTerminal ioVm

vmNew :: Vm
vmNew = ("get", "", 200, [], False)

vmDefault :: IO Vm
vmDefault = do return ("get", "", 200, [], False)

vmFromk :: Policy -> [Instr] -> IO Vm
vmFromk Policy { maxReruns, maxRetriesPerCall, timeMillisPerCall } instrs = do
    return ("get", "", 200, instrs, False)

vmFrom :: [Instr] -> IO Vm
vmFrom instrs = do return ("get", "", 200, instrs, False)

-- vmVerb :: Vm -> HttpVerb
-- vmVerb (i, _, _, _, _) = i
-- vmUrl :: Vm -> Url
-- vmUrl (_, j, _, _, _) = j
-- vmExpect :: Vm -> ExpectCode
-- vmExpect (_, _, k, _, _) = k
-- vmInstrs :: Vm -> [Instr]
-- vmInstrs (_, _, _, l, _) = l
-- vmTerminal :: Vm -> Terminal
-- vmTerminal (_, _, _, _, m) = m

-- Progress by executing top Instr.
step :: IO Vm -> IO Vm
step ioVm = do
    instr <- peekInstr ioVm
    go instr where
        go NOP    = popInstr ioVm
        go IV     = setVerb ioVm "get"
        go IC     = setExpectCode ioVm 200
        go (OV s) = setVerb ioVm s
        go X      = executeVerb ioVm
        go _      = vmDefault

type CodesMatch = Bool
ioPost :: Vars -> HttpMethod -> Url -> IO CodesMatch
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

    return False

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
