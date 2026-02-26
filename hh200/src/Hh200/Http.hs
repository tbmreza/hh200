module Hh200.Http
  -- ( Manager
  -- , Request
  -- , Response
  -- , RequestBody
  -- , HttpException
  -- , newManager
  -- , closeManager
  -- , parseRequest
  -- , httpLbs
  -- , setMethod
  -- , setRequestHeaders
  -- , setRequestBody
  -- , lbsBody
  -- , getStatus
  -- , getBody
  -- , getHeaders
  -- ) where
    where

import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Client.TLS as HCT
import           Network.HTTP.Types.Status (Status)
import           Network.HTTP.Types.Header (HeaderName, ResponseHeaders)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS

-- Guessed (any benchmark results will validate or override) criteria for
-- Manager sharing:
-- 1. Unchanging hosts: fully analyzed hhs script can tell when to instantiate
--                      new Manager.
-- 2. Load test mode
--
-- ??: stack build config to switch between http client libs, goal: fork development exit mechanism + guide general (presumably more correct) api
-- ??: config respect-url-insecure-http, leaving it false means always use TLS
type Manager = HC.Manager
type Request = HC.Request
type Response = HC.Response LBS.ByteString
type RequestBody = HC.RequestBody
type HttpException = HC.HttpException

-- newManager :: Bool -> IO Manager
-- newManager useTls = if useTls
--     then HC.newManager HCT.tlsManagerSettings
--     else HC.newManager HC.defaultManagerSettings
newManager :: Bool -> IO Manager
newManager useTls = if useTls
    then HC.newManager HCT.tlsManagerFork
    else HC.newManager HC.defaultManagerSettings

closeManager :: Manager -> IO ()
closeManager = HC.closeManager

parseRequest :: String -> IO Request
parseRequest = HC.parseRequest

httpLbs :: Request -> Manager -> IO Response
httpLbs = HC.httpLbs

getStatus :: Response -> Status
getStatus = HC.responseStatus

getBody :: Response -> LBS.ByteString
getBody = HC.responseBody

getHeaders :: Response -> ResponseHeaders
getHeaders = HC.responseHeaders
