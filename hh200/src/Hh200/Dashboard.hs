{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hh200.Dashboard
  ( startServer
  , dashboardRoutes
  , controlSocketPath
  , controlSocketWriter
  ) where

import           Control.Concurrent (threadDelay)
import           Control.Monad (forever, unless)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import           Data.Aeson (object, encode, (.=))
import           Data.Maybe (fromMaybe)
import           Data.Text (unpack)
import qualified Data.Text.Lazy as LT
import           Network.HTTP.Types.Status (status404)
import           Network.Socket
import qualified Network.Socket.ByteString as NBS
import           Network.Wai.Middleware.Static (staticPolicy, addBase)
import           System.Environment (lookupEnv)
import           Web.Scotty (ScottyM, scotty, regex, pathParam)
import qualified Web.Scotty as Server

import           Hh200.Database (initDb, listRuns, queryStatsHistory)

--------------------------------------------------------------------------------
-- Dashboard server
--------------------------------------------------------------------------------
-- -- Frontend-initiated
-- GET /api/runs           List runs from SQLite
-- GET /api/runs/live      In-progress run at the time being (not implemented)
-- GET /api/runs/:runId    Completed run detail (not implemented)
-- GET /api/report/:runId  Download report for a Run in CSV
--
-- -- Backend-initiated
-- GET /sse                SSE stream for live data
-- GET /api/sse            SSE stream for live data
--
-- -- Browse mode
-- POST /sig               Receives pause/resume/stop, forwards to UDS
-- GET /*                  SvelteKit static SPA

startServer :: String -> IO ()
startServer portStr = do
    isDev <- fmap (== Just "1") (lookupEnv "DEV")
    let port = read portStr :: Int
    putStrLn $ "hh200 dashboard listening on http://localhost:" ++ show port
    scotty port (dashboardRoutes isDev)

-- | The dashboard's WAI route table, parameterized by dev mode so tests can
-- exercise both the static-file (prod) and no-static-file (dev) behavior.
dashboardRoutes :: Bool -> ScottyM ()
dashboardRoutes isDev = do
    unless isDev $
        Server.middleware (staticPolicy (addBase "min"))

    Server.get "/api/runs" $ do
        conn <- liftIO initDb
        rows <- liftIO $ listRuns conn
        Server.json $ object ["runs" .= rows]

    Server.get "/api/report/:runId" $ do
        conn <- liftIO initDb
        runId :: Int <- pathParam "runId"

        results <- liftIO $ queryStatsHistory conn runId

        let downloadName = LT.pack ("stats_history_" ++ show runId ++ ".csv")
        case results of
            Left err -> do
                Server.status status404
                Server.json $ object ["error" .= unpack err]

            Right stats -> do
                let bs = Csv.encodeDefaultOrderedByName stats
                Server.setHeader "Content-Type" "text/csv"
                Server.setHeader "Content-Disposition" ("attachment; filename=\"" <> downloadName <> "\"")
                Server.raw bs

    Server.post "/api/sig" $ do
        body :: BL.ByteString <- Server.body

        path <- liftIO controlSocketPath
        liftIO $ controlSocketWriter path body

        Server.json $ object ["ok" .= ((show body) :: String)]

    -- Server.get "/sse" $ do
    Server.get "/api/sse" $ do
        Server.setHeader "Content-Type" "text/event-stream"
        Server.setHeader "Cache-Control" "no-cache"
        -- ??: type Event = NoNews | NewRow | UpdateRow
        Server.stream $ \send flush -> forever $ do
            let evt = object
                    [ "nested" .= (object ["inner" .= (1 :: Int)])
                    , "float" .= (1.5 :: Double)
                    , "array" .= ([1, 2, 3] :: [Int])
                    , "string" .= ("hello" :: String)
                    , "bool" .= True
                    ]
                frame = "data: " <> encode evt <> "\n\n"
            send (B.lazyByteString frame)
            flush
            threadDelay 2000000  -- ??: event every 1.5s

    -- Catch-all GET for SvelteKit pages routes.
    Server.get (regex "(.*)") $
        unless isDev (Server.file "min/200.html")


-- | Path of the Unix domain socket that receives pause/resume/stop signals.
-- Overridable via @HH200_CONTROL_SOCKET@ so tests can point it at a socket
-- they own.
controlSocketPath :: IO FilePath
controlSocketPath = fromMaybe "/tmp/uds_socket" <$> lookupEnv "HH200_CONTROL_SOCKET"


controlSocketWriter :: FilePath -> BL.ByteString -> IO ()
controlSocketWriter path msg = do
    sock <- socket AF_UNIX Stream defaultProtocol
    connect sock (SockAddrUnix path)
    _ <- NBS.send sock (BL.toStrict msg)
    close sock
