{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Hh200.Database
    ( Run(..)
    , Request(..)
    , TcpEvent(..)
    , DbEvent(..)
    , initDatabase
    , startDbWriter
    ) where

import           Control.Concurrent (forkIO, ThreadId)
import           Control.Concurrent.STM
import           Control.Monad (forever, void, when)
import           Data.Int (Int64)
import           Data.Text (Text)
import           Database.SQLite.Simple
import           Database.SQLite.Simple.ToField
import           GHC.Generics

data Run = Run
    { runId     :: Text
    , startedAt :: Int64
    , config    :: Text -- JSON
    } deriving (Show, Generic)

instance ToRow Run where
    toRow (Run rid sa cfg) = toRow (rid, sa, cfg)

data Request = Request
    { reqRunId      :: Text
    , requestId     :: Text
    , workerId      :: Int
    , srcPort       :: Int
    , url           :: Text
    , method        :: Text
    , status        :: Int
    , bodyBytes     :: Int
    , tStart        :: Int64
    , tEnd          :: Int64
    , durationMs    :: Double
    } deriving (Show, Generic)

instance ToRow Request where
    toRow (Request rid reqid wid port u m s b ts te d) =
        [ toField rid
        , toField reqid
        , toField wid
        , toField port
        , toField u
        , toField m
        , toField s
        , toField (fromIntegral b :: Int)
        , toField ts
        , toField te
        , toField d
        ]

data TcpEvent = TcpEvent
    { tcpRunId     :: Text
    , tcpSrcPort   :: Int
    , tcpEventType :: Text
    , tcpValue     :: Double
    , kernelTs     :: Int64
    } deriving (Show, Generic)

instance ToRow TcpEvent where
    toRow (TcpEvent rid port et v kts) = toRow (rid, port, et, v, kts)

data DbEvent
    = AddRun Run
    | AddRequest Request
    | AddTcpEvent TcpEvent
    deriving (Show)

initDatabase :: Connection -> IO ()
initDatabase conn = do
    execute_ conn "DROP TABLE IF EXISTS timeseries_data"
    execute_ conn "CREATE TABLE IF NOT EXISTS runs (run_id TEXT PRIMARY KEY, started_at INTEGER, config TEXT)"
    execute_ conn "CREATE TABLE IF NOT EXISTS requests (id INTEGER PRIMARY KEY, run_id TEXT, request_id TEXT, worker_id INTEGER, src_port INTEGER, url TEXT, method TEXT, status INTEGER, body_bytes INTEGER, t_start INTEGER, t_end INTEGER, duration_ms REAL)"
    execute_ conn "CREATE TABLE IF NOT EXISTS tcp_events (id INTEGER PRIMARY KEY, run_id TEXT, src_port INTEGER, event_type TEXT, value REAL, kernel_ts INTEGER)"

startDbWriter :: Connection -> TQueue (Maybe DbEvent) -> IO ThreadId
startDbWriter conn q = forkIO $ loop
    where
    loop = do
        mEvent <- atomically $ readTQueue q
        case mEvent of
            Nothing -> pure () -- Termination signal
            Just event -> do
                case event of
                    AddRun run -> execute conn "INSERT INTO runs (run_id, started_at, config) VALUES (?, ?, ?)" run
                    AddRequest req -> execute conn "INSERT INTO requests (run_id, request_id, worker_id, src_port, url, method, status, body_bytes, t_start, t_end, duration_ms) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)" req
                    AddTcpEvent ev -> execute conn "INSERT INTO tcp_events (run_id, src_port, event_type, value, kernel_ts) VALUES (?, ?, ?, ?, ?)" ev
                loop
