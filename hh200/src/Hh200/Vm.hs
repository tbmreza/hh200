{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Hh200.Vm where

import Prelude
import GHC.Generics (Generic)
import System.IO (withFile, IOMode(WriteMode))
import Control.Exception (bracket, Exception, throwIO)
import Control.Monad (unless)
import Control.Monad.Identity

import           Data.Aeson (FromJSON, ToJSON, Value, eitherDecode, encode, decode)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Yaml as Yaml
import qualified Data.ByteString.Lazy as L
import           Data.List (unsnoc)
import           Data.List.Split (splitOn)

import           Network.HTTP.Client
import qualified Network.HTTP.Client as H
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.HTTP.Simple (RequestHeaders)
import           Network.HTTP.Types.Status (statusCode)

import Hh200.Types


type Terminal = Bool
type RawString = ByteString
type Vm = (HttpVerb, RequestHeaders, Url, RawString, ExpectCode, [Instr], Terminal)

class VmT a where
    setTerminal :: IO a -> IO a
    peekInstr :: a -> Maybe Instr
    popInstr :: a -> (Maybe InternalError, a)
    setVerb :: a -> HttpVerb -> a
    setExpectCode :: IO a -> ExpectCode -> IO a
    setParametrizedUrl :: IO a -> Url -> IO a
    httpClientCall :: a -> IO (Maybe InternalError)

instance VmT Vm where
    setTerminal ioVm = do
        (e0, e1, e2, e3, e4, e5, _) <- ioVm
        return (e0, e1, e2, e3, e4, e5, True)

    setVerb (_, e1, e2, e3, e4, e5, e6) v =
        (v, e1, e2, e3, e4, e5, e6)

    setParametrizedUrl ioVm u = do
        (e0, e1, _, e3, e4, e5, e6) <- ioVm
        return (e0, e1, u, e3, e4, e5, e6)

    setExpectCode ioVm c = do
        (e0, e1, e2, e3, _, e5, e6) <- ioVm
        return (e0, e1, e2, e3, c, e5, e6)

    peekInstr (_, _, _, _, _, instrs, _) =
        case instrs of
            [] -> Nothing
            instrs -> Just $ head instrs

    popInstr (e0, e1, e2, e3, e4, instrs, e6) = do
        case instrs of
            [] -> (Just OutOfBounds, (e0, e1, e2, e3, e4, [], e6))
            _ -> (Nothing, (e0, e1, e2, e3, e4, tail instrs, e6))


    httpClientCall (verb, headers, url, raw, expectCode, instrs, terminal) = do
        manager <- newManager tlsManagerSettings
        request <- parseRequest url
        bracket
            -- Acquire and defer release.
            (H.httpLbs request manager) responseClose

            -- Work with it.
            (\response -> do
                let status = responseStatus response
                if statusCode status == 200
                    then withFile (filenamePartOrDefault url) WriteMode (\handle -> L.hPut handle (responseBody response))
                    else putStrLn ("Failed to download file. Status code: " ++ show status))
        return Nothing
        where
        filenamePartOrDefault :: String -> String
        filenamePartOrDefault url =
            let splitted = splitOn "/" url in
            case unsnoc splitted of
                Nothing -> "out"  -- ??: if default turns out to be common enough case, maybe name the file with timestamp
                Just (_, part) -> part


-- ??: Effects of http, reporting, concurrency, throwing,
step :: Vm -> IO (Maybe InternalError, Vm)
step state = do
    print "step..."
    let (e0, e1, e2, e3, e4, e5, e6) = state
    case peekInstr state of
        Nothing ->
            return (Nothing, ("", [], "", "", 0, [], True))
        Just instr -> go instr where
            go NOP =
                return $ popInstr state

            go (SH "" []) =
                return (Nothing, state)  -- ??: spread RequestHeaders

            go X = do
                clientError <- httpClientCall state
                -- ??: match clientError, maybe bubble it up
                return $ popInstr state

            go (MATCH_CODES n) =
                -- ??: report span of status code literal
                return (Nothing, (e0, e1, e2, e3, e4, e5, True))

            go _ =
                return (Just Todo, state)

type CodesMatch = Bool

-- Virtual machine is final if previous state set it to be Terminal
-- or if there's no [Instr] left.
isFinal :: Vm -> Bool
isFinal (_, _, _, _, _, _, True) = True
isFinal (_, _, _, _, _, [], _) = True
isFinal _ = False

type Eval1 a = Identity a

-- runEval1 :: Eval1 a -> a
-- runEval1 ev = runIdentity ev
--
-- eval1                   ::  Env -> Exp -> Eval1 Value
--
-- runEval5            ::  Env -> Integer -> Eval5 a -> ((Either String a, [String]), Integer)
-- runEval5 env st ev  =
--     runIdentity (runStateT (runWriterT (runExceptT (runReaderT ev env))) st)
--
-- eval5               ::  Exp -> Eval5 Value
-- data VmState = FinalVm | LiveVm
-- eval :: VmState -> Eval VmState

vmRun :: Vm -> IO Vm
vmRun vm = do
    fwd <- step vm
    case fwd of
        (Just _err, _) -> throwIO TerribleException
        (Nothing, state) -> if isFinal state
            then return state
            else vmRun state
