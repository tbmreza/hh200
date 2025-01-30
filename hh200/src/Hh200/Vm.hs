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
import           Network.HTTP.Simple
import           Network.HTTP.Types.Status (statusCode)

import Hh200.Types

filenamePartOrDefault :: String -> String
filenamePartOrDefault url =
    let splitted = splitOn "/" url in
    case unsnoc splitted of
        Nothing -> "out"  -- ??: if default turns out to be common enough case, maybe name the file with timestamp
        Just (_, part) -> part

downloadFile :: String -> IO ()
downloadFile url = do
  manager <- newManager tlsManagerSettings
  request <- parseRequest url
  bracket
    (H.httpLbs request manager)
    responseClose
    (\response -> do
      let status = responseStatus response
      if statusCode status == 200
        then withFile (filenamePartOrDefault url) WriteMode (\handle -> L.hPut handle (responseBody response))
        else putStrLn ("Failed to download file. Status code: " ++ show status)
    )

type Terminal = Bool
type RawString = ByteString
type Vm = (HttpVerb, RequestHeaders, Url, RawString, ExpectCode, [Instr], Terminal)

-- -- -- type MaybeJson = String
-- -- -- screen :: String -> IO MaybeJson
-- -- -- screen :: Text -> IO Text
--
-- -- ??: aka isValidJson
-- -- screen :: FromJSON a => String -> IO String
-- -- -- screen _ = do return ""
-- -- screen jsonString = do
-- --     -- case eitherDecode (T.encodeUtf8 jsonString) of
-- --     case (eitherDecode (BS.pack jsonString) :: Either String a) of
-- --         -- Left _ -> return T.empty
-- --         -- Right _ -> return jsonString
-- --         Left _ -> return ""
-- --         Right _ -> return jsonString
-- --
-- -- downstream :: IO ()
-- -- downstream = do
-- --     let validJson = "{\"name\": \"John\", \"age\": 30}"
-- --     unw <- screen validJson
-- --     putStrLn $ "Is valid JSON: " ++ show unw
--
-- -- To decode or encode a value using the generic machinery, we must
-- -- make the type an instance of the Generic class.
-- data Coord = Coord { x :: Double, y :: Double }
--              deriving (Show, Generic)
--
-- -- While we still have to declare our type as instances of FromJSON
-- -- and ToJSON, we do *not* need to provide bodies for the instances.
-- -- Default versions will be supplied for us.
--
-- instance FromJSON Coord
-- instance ToJSON Coord
--
-- downstream :: IO ()
-- downstream = do
--     -- let req = decode "{\"x\":3.0,\"y\":-1.0}" :: Maybe Coord
--     let req = decode "{\"x\":lit,\"y\":-1.0" :: Maybe Coord
--     print req
--     let reply = Coord { x = 123.4, y = 20 }
--     BS.putStrLn (encode reply)

class VmT a where
    setTerminal :: IO a -> IO a
    peekInstr :: a -> Maybe Instr
    popInstr :: a -> (Maybe InternalError, a)
    setVerb :: a -> HttpVerb -> a
    setExpectCode :: IO a -> ExpectCode -> IO a
    setParametrizedUrl :: IO a -> Url -> IO a
    executeVerb :: a -> IO (Maybe InternalError, a)
    httpClientCall :: a -> IO (CodesMatch, a)

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

    -- (_, _, _, X : rest, False)
    -- setRequestMethod :: S.ByteString -> H.Request -> H.Request
    -- setRequestMethod x req = req { H.method = x }
    --
    -- setRequestHeaders :: H.RequestHeaders -> H.Request -> H.Request
    -- setRequestHeaders x req = req { H.requestHeaders = x }

    -- Objective: http://localhost:8787/product/1222/first
    -- with ("GET", [], url, 200, X : rest, False)
    --

    -- State progression handled at callsite.
    httpClientCall (verb, headers, url, raw, expectCode, instrs, terminal) = do
        build <- parseRequest url
        let req = build { H.method = verb
                        -- , H.requestBody = RequestBodyLBS $ encode requestObject
                        , H.requestBody = RequestBodyLBS raw
                        -- H.requestHeaders = [ ("Content-Type", "application/json; charset=utf-8") ]
                        , H.requestHeaders = headers
                        }
        -- Script isn't always expressing concerns about responses that it is receiving, so
        -- we're picking lazy ByteString on the assumption that it gives better performance
        -- on average than the strict one. Probably we'll end up providing both (or neither).
        resp <- httpLBS req

        putStrLn $ BS.unpack $ getResponseBody resp

        -- next <- ioVm
        return (getResponseStatusCode resp == expectCode, (verb, headers, url, raw, expectCode, instrs, terminal))

    -- popInstr to bump state on success, otherwise also do required housekeeping.
    --
    executeVerb (e0, e1, e2, e3, e4, instr:rest, e6) = do
        case instr of
            HARDCODE -> do  -- ??:
                called <- downloadFile e2
                -- return (Nothing, (e0, e1, e2, e3, e4, instr:rest, e6))
                return (Nothing, (e0, e1, e2, e3, e4, instr:rest, True))
            X -> do
                called <- httpClientCall (e0, e1, e2, e3, e4, instr:rest, e6)
                triage called
            _ -> do
                return (Just Todo, (e0, e1, e2, e3, e4, instr:rest, e6))

            where
                triage (True, state) = do
                    return $ popInstr state

                triage (False, state) = do
                    -- ??: some housekeeping on unexpected http response
                    return $ popInstr state

                -- triage _ = do  -- ??:
                --     return (Nothing, s)

    executeVerb (e0, e1, e2, e3, e4, [], e6) = return (Just OutOfBounds, (e0, e1, e2, e3, e4, [], e6))

-- vmDefault :: IO Vm
-- vmDefault = do return ("GET", [], "", "", 200, [], False)
--
-- vmFrom :: Policy -> [Instr] -> IO Vm
-- vmFrom Policy { maxReruns, maxRetriesPerCall, timeMillisPerCall } instrs = do
--     return ("GET", [], "", "", 200, [], False)

stepk :: Vm -> IO (Maybe InternalError, Vm)
stepk state = do
    case peekInstr state of
        Nothing ->
            -- Should have been unreachable.
            return (Just OutOfBounds, state)
        Just instr -> go instr where
            go IV =
                return (ok, setVerb state "GET")
            go NOP =
                return $ popInstr state
            go X =
                executeVerb state
            go HARDCODE =
                executeVerb state
            go _ =
                return (Just Todo, state)

-- step :: IO Vm -> IO (Maybe InternalError, Vm)
-- step ioVm = do
--     state <- ioVm
--     case peekInstr state of
--         Nothing ->
--             -- Should have been unreachable.
--             return (Just OutOfBounds, state)
--         Just instr -> go instr where
--             go IV =
--                 return (ok, setVerb state "GET")
--             go NOP =
--                 return $ popInstr state
--             go X =
--                 executeVerb state
--             go _ =
--                 return (Just Todo, state)
--
-- -- -- Progress by executing top Instr.
-- -- step :: IO Vm -> IO Vm
-- -- step ioVm = do
-- --     peeked <- peekInstr ioVm
-- --     case peeked of
-- --         Nothing -> do
-- --             putStrLn "todo warn: bunreachable"
-- --             vmDefault
-- --         Just instr -> go instr where
-- --             go IV     = setVerb ioVm "GET"         -- infallible
-- --             go IC     = setExpectCode ioVm 200     -- infallible
-- --             go (OV s) = setVerb ioVm s             -- infallible
-- --             go (OC s) = setExpectCode ioVm s       -- infallible
-- --             go (SU s) = setParametrizedUrl ioVm s  -- infallible
-- --             go NOP    = do
-- --                 (_, next) <- popInstr ioVm
-- --                 return next
-- --             go X      = executeVerb ioVm
--
--
type CodesMatch = Bool
-- ioPost :: Vars -> HttpMethod -> Url -> IO CodesMatch
-- ioPost _ methd url = do
--     -- request' <- parseRequest (methd ++ url)
--     request' <- parseRequest url
--     let request
--             -- = setRequestPort 443
--             -- = setRequestPort 8787
--             = setRequestMethod methd
--             -- = setRequestMethod "POST"
--             -- $ setRequestPath "/put"
--             $ setRequestQueryString [("hello", Just "world")]
--             $ setRequestBodyLBS "This is my request body"
--             -- $ setRequestSecure True
--             $ request'
--     -- response <- httpJSON request
--     --
--     -- putStrLn $ "The status code was: " ++
--     --            show (getResponseStatusCode response)
--     -- print $ getResponseHeader "Content-Type" response
--     -- C8.putStrLn $ Yaml.encode (getResponseBody response :: Value)
--
--     return False

-- Virtual machine is final if previous state set it to be Terminal
-- or if there's no [Instr] left.
isFinal :: Vm -> Bool
isFinal (_, _, _, _, _, _, True) = True
isFinal (_, _, _, _, _, [], _) = True
isFinal _ = False

-- parseHhs :: Source -> IO [Instr]
-- parseHhs _ = do
--     let sourceNotValid = False
--     if sourceNotValid
--         then do
--             putStrLn "todo error"
--             return []
--         else do return [SU "http://httpbin.org/anything", X]
--
-- -- toVm :: "examples/sample.http" -> Maybe Vm
-- toVm :: FilePath -> Maybe Vm
-- toVm _ = Just ("OPTIONS", [], "http://localhost:9999/ignore.php", "ignore...", 200, [X], False)
--

-- type Eval1 alpha  =   Identity alpha
--
-- runEval1 :: Eval1 alpha -> alpha
-- runEval1 ev = runIdentity ev
--
-- eval1                   ::  Env -> Exp -> Eval1 Value
--
-- runEval5            ::  Env -> Integer -> Eval5 alpha -> ((Either String alpha, [String]), Integer)
-- runEval5 env st ev  =
--     runIdentity (runStateT (runWriterT (runExceptT (runReaderT ev env))) st)
--
-- eval5               ::  Exp -> Eval5 Value
-- data VmState = FinalVm | LiveVm
-- eval :: VmState -> Eval VmState

vmRun :: Vm -> IO Vm
vmRun vm = do
    fwd <- stepk vm
    case fwd of
        (Just _, _) -> throwIO TerribleException
        (Nothing, state) -> if isFinal state
            then return state
            else vmRun state
