module Hh200.Http
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
-- ??: housekeep warnings; a good prompt for claude code to refactor current Http backend module that I have to such record of closures
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
    -- then HC.newManager HCT.tlsManagerFork
    -- ??
    then HC.newManager HC.defaultManagerSettings
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

getCookieJar :: Response -> HC.CookieJar
getCookieJar = HC.responseCookieJar
