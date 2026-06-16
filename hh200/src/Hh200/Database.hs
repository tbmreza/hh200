{-# LANGUAGE OverloadedStrings #-}

module Hh200.Database
  ( RunId
  -- , MetricRow
  -- , BpfRow
  -- , RunMeta
  , initDb
  , closeDb
  ) where

import Debug.Trace

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Database.SQLite.Simple (Connection, ToRow, FromRow, toRow, fromRow, execute_, execute, close, open, field)
import           Network.HTTP.Types.Header (HeaderName)
import           System.Directory (XdgDirectory (XdgData), getXdgDirectory)
import           System.Environment (lookupEnv)
import           System.Exit (exitSuccess)
import           System.FilePath ((<.>), (</>))


newtype RunId = RunId Int
-- MetricRow   -- a completed request's recorded data
-- BpfRow      -- a BPF event row (placeholder shape for now)
-- RunMeta     -- metadata about a test run (start time, config snapshot, etc.)


-- (auto)
initDb :: IO Connection
initDb = do
    mPath <- lookupEnv "HH200_SQLITE"
    case mPath of
        Just fp -> open (trace ("mPath=" ++ fp) fp)
        Nothing -> do
            dir <- getXdgDirectory XdgData "hh200"
            putStrLn dir
            exitSuccess

closeDb :: Connection -> IO ()
closeDb = close

-- insertRun      :: Connection -> RunMeta -> IO RunId
-- insertRun = undefined
--
-- insertMetric   :: Connection -> RunId -> MetricRow -> IO ()
-- insertMetric = undefined
--
-- insertMetrics  :: Connection -> RunId -> [MetricRow] -> IO ()
-- insertMetrics = undefined
--
-- insertBpfEvent :: Connection -> RunId -> BpfRow -> IO ()
-- insertBpfEvent = undefined

-- getRun          :: Connection -> RunId -> IO (Maybe RunMeta)
-- getRun = undefined
-- listRuns        :: Connection -> IO [RunMeta]
-- listRuns = undefined
-- getMetrics      :: Connection -> RunId -> IO [MetricRow]
-- getMetrics = undefined
-- getMetricsSince :: Connection -> RunId -> UTCTime -> IO [MetricRow]
-- getMetricsSince = undefined
-- getBpfEvents    :: Connection -> RunId -> IO [BpfRow]
-- getBpfEvents = undefined

-- aggLatencyPercentiles :: Connection -> RunId -> IO LatencyStats
-- aggErrorRate          :: Connection -> RunId -> IO ErrorStats
-- aggThroughput         :: Connection -> RunId -> IO ThroughputStats
