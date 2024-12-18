{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Hh200.Vm where

import Prelude.Compat
import GHC.Generics (Generic)

import Control.Monad (unless)
import           Data.Aeson            (FromJSON, ToJSON, Value, eitherDecode, encode, decode)
import qualified Data.HashMap.Strict   as HM
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS

-- import Data.ByteString.Internal.Type
-- import qualified Data.ByteString       as S8
-- import qualified Data.ByteString.Char8 as C8

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Yaml             as Yaml
import           Network.HTTP.Simple
import           Network.HTTP.Client
import Hh200.Types

import qualified Network.HTTP.Client as H

type Terminal = Bool
type RawString = ByteString
type Vm = (HttpVerb, RequestHeaders, Url, RawString, ExpectCode, [Instr], Terminal)
-- type Vm = (HttpVerb, RequestHeaders, Url, ExpectCode, [Instr], Terminal)

-- -- type MaybeJson = String
-- -- screen :: String -> IO MaybeJson
-- -- screen :: Text -> IO Text

-- ??: aka isValidJson
-- screen :: FromJSON a => String -> IO String
-- -- screen _ = do return ""
-- screen jsonString = do
--     -- case eitherDecode (T.encodeUtf8 jsonString) of
--     case (eitherDecode (BS.pack jsonString) :: Either String a) of
--         -- Left _ -> return T.empty
--         -- Right _ -> return jsonString
--         Left _ -> return ""
--         Right _ -> return jsonString
--
-- downstream :: IO ()
-- downstream = do
--     let validJson = "{\"name\": \"John\", \"age\": 30}"
--     unw <- screen validJson
--     putStrLn $ "Is valid JSON: " ++ show unw

-- To decode or encode a value using the generic machinery, we must
-- make the type an instance of the Generic class.
data Coord = Coord { x :: Double, y :: Double }
             deriving (Show, Generic)

-- While we still have to declare our type as instances of FromJSON
-- and ToJSON, we do *not* need to provide bodies for the instances.
-- Default versions will be supplied for us.

instance FromJSON Coord
instance ToJSON Coord

downstream :: IO ()
downstream = do
    -- let req = decode "{\"x\":3.0,\"y\":-1.0}" :: Maybe Coord
    let req = decode "{\"x\":lit,\"y\":-1.0" :: Maybe Coord
    print req
    let reply = Coord { x = 123.4, y = 20 }
    BS.putStrLn (encode reply)

class VmT a where
    setTerminal :: IO a -> IO a
    peekInstr :: IO a -> IO (Maybe Instr)
    -- popInstr :: IO a -> IO a
    popInstr :: IO a -> IO (Maybe InternalError, a)
    setVerb :: IO a -> HttpVerb -> IO a
    setExpectCode :: IO a -> ExpectCode -> IO a
    setParametrizedUrl :: IO a -> Url -> IO a
    executeVerb :: IO a -> IO a
    httpClientCall :: IO a -> IO (CodesMatch, a)

instance VmT Vm where
    setTerminal ioVm = do
        (e0, e1, e2, e3, e4, e5, _) <- ioVm
        return (e0, e1, e2, e3, e4, e5, True)

    setVerb ioVm v = do
        (_, e1, e2, e3, e4, e5, e6) <- ioVm
        return (v, e1, e2, e3, e4, e5, e6)

    setParametrizedUrl ioVm u = do
        (e0, e1, _, e3, e4, e5, e6) <- ioVm
        return (e0, e1, u, e3, e4, e5, e6)

    setExpectCode ioVm c = do
        (e0, e1, e2, e3, _, e5, e6) <- ioVm
        return (e0, e1, e2, e3, c, e5, e6)

    peekInstr ioVm = do
        (_, _, _, _, _, instrs, _) <- ioVm
        return $ case instrs of
            [] -> Nothing
            instrs -> Just $ head instrs

    popInstr ioVm = do
        (e0, e1, e2, e3, e4, instrs, e6) <- ioVm

        return $ case instrs of
            -- has err: no tail
            [] -> (Just OutOfBounds, (e0, e1, e2, e3, e4, [], e6))
            _ -> (Nothing, (e0, e1, e2, e3, e4, tail instrs, e6))

    -- popInstr ioVm = do
    --     (e0, e1, e2, e3, e4, instrs, e6) <- ioVm
    --     return (e0, e1, e2, e3, e4, tail instrs, e6)


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

    -- hcc ioVm = do
    httpClientCall ioVm = do
        (verb, headers, url, raw, expectCode, instrs, terminal) <- ioVm
        -- (verb, headers, url, expectCode, instrs, terminal) <- ioVm
        build <- parseRequest url
        let req = build { H.method = verb
                        -- , H.requestBody = RequestBodyLBS $ encode requestObject
                        , H.requestBody = RequestBodyLBS raw
                        -- H.requestHeaders = [ ("Content-Type", "application/json; charset=utf-8") ]
                        , H.requestHeaders = headers
                        }

        -- putStrLn $ show req

        -- Script isn't always expressing concerns about responses that it is receiving, so
        -- we're picking lazy ByteString on the assumption that it gives better performance
        -- on average than the strict one. Probably we'll end up providing both (or neither).
        resp <- httpLBS req

        putStrLn $ BS.unpack $ getResponseBody resp

        next <- ioVm
        return (getResponseStatusCode resp == expectCode, next)

    -- Execute machine's state.
    -- popInstr on success and continue, otherwise stop everything gracefully
    -- ??: unfit state is an error, communicate that
    executeVerb ioVm = do
        -- Whether state is fit for httpClientCall.
        (e0, e1, e2, e3, e4, instrs, e6) <- ioVm
        case (e0, e1, e2, e3, e4, instrs, e6) of
            -- The only caller at the moment is `step ioVm`.
            (e0, e1, e2, e3, e4, _xHeaded, False) -> do
                (codesMatch, next) <- httpClientCall ioVm
                return next
            _ -> do
                putStrLn "todo warn: should be unreachable"
                -- next <- popInstr ioVm
                (_, next) <- popInstr ioVm
                return next

vmDefault :: IO Vm
vmDefault = do return ("GET", [], "", "", 200, [], False)

vmFrom :: Policy -> [Instr] -> IO Vm
vmFrom Policy { maxReruns, maxRetriesPerCall, timeMillisPerCall } instrs = do
    return ("GET", [], "", "", 200, [], False)

-- Progress by executing top Instr.
step :: IO Vm -> IO Vm
step ioVm = do
    peeked <- peekInstr ioVm
    case peeked of
        Nothing -> do
            putStrLn "todo warn: bunreachable"
            vmDefault
        Just instr -> go instr where
            go IV     = setVerb ioVm "GET"         -- infallible
            go IC     = setExpectCode ioVm 200     -- infallible
            go (OV s) = setVerb ioVm s             -- infallible
            go (OC s) = setExpectCode ioVm s       -- infallible
            go (SU s) = setParametrizedUrl ioVm s  -- infallible
            go NOP    = do
                (_, next) <- popInstr ioVm
                return next
            go X      = executeVerb ioVm

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
    -- response <- httpJSON request
    --
    -- putStrLn $ "The status code was: " ++
    --            show (getResponseStatusCode response)
    -- print $ getResponseHeader "Content-Type" response
    -- C8.putStrLn $ Yaml.encode (getResponseBody response :: Value)

    return False

-- Virtual machine is final if previous state set it to be Terminal
-- or if there's no [Instr] left.
isFinal :: Vm -> Bool
isFinal (_, _, _, _, _, _, True) = True
isFinal (_, _, _, _, _, [], _) = True
isFinal _ = False

parseHhs :: Source -> IO [Instr]
parseHhs _ = do
    let sourceNotValid = False
    if sourceNotValid
        then do
            putStrLn "todo error"
            return []
        else do return [SU "http://httpbin.org/anything", X]

-- untilIO :: (a -> IO Bool) -> (a -> a) -> a -> a
-- untilIO p f = go
--   where
--     go x | p x          = x
--          | otherwise    = go (f x)

vmRun :: IO Vm -> IO Vm
vmRun ioVm = do
    -- PICKUP if err throw exception
    -- (err, state) <- step ioVm
    state <- step ioVm
    if isFinal state
        then return state
        else vmRun $ return state
