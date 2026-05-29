{-# LANGUAGE OverloadedStrings #-}

module Hh200.Database
  ( RunId
  -- , MetricRow
  -- , BpfRow
  -- , RunMeta
  , initDb
  , closeDb
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Network.HTTP.Types.Header (HeaderName)
import           System.FilePath ((<.>), (</>))
import           Database.SQLite.Simple (Connection, ToRow, FromRow, toRow, fromRow, execute_, execute, close, open, field)


newtype RunId = RunId Int
-- MetricRow   -- a completed request's recorded data
-- BpfRow      -- a BPF event row (placeholder shape for now)
-- RunMeta     -- metadata about a test run (start time, config snapshot, etc.)


initDb :: FilePath -> IO Connection
initDb = undefined

closeDb :: Connection -> IO ()
closeDb = undefined

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
