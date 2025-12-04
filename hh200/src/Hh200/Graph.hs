module Hh200.Graph
  ( connect
  ) where

import           Control.Exception              (bracket)
import           Control.Monad                  (void)
import           Data.Int                       (Int64)
import           Data.Time.Clock.POSIX          (getPOSIXTime)
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow

data TimeseriesData = TimeseriesData
  { timestamp :: Int64
  , value     :: Double
  } deriving (Eq, Show)

instance FromRow TimeseriesData where
  fromRow = TimeseriesData <$> field <*> field

instance ToRow TimeseriesData where
  toRow (TimeseriesData ts val) = toRow (ts, val)

connect :: FilePath -> IO ()
connect dbPath =
  bracket (open dbPath) close $ \conn -> do
    putStrLn $ "Connected to SQLite database: " ++ dbPath
    createTable conn
    insertDummyData conn
    putStrLn "Dummy data inserted."

createTable :: Connection -> IO ()
createTable conn =
  execute_ conn "CREATE TABLE IF NOT EXISTS timeseries_data (timestamp INTEGER PRIMARY KEY, value REAL)"

insertDummyData :: Connection -> IO ()
insertDummyData conn = do
  void $ execute conn "INSERT INTO timeseries_data (timestamp, value) VALUES (?, ?)" (1678886400 :: Int64, 10.5 :: Double)
  void $ execute conn "INSERT INTO timeseries_data (timestamp, value) VALUES (?, ?)" (1678886460 :: Int64, 12.3 :: Double)
  void $ execute conn "INSERT INTO timeseries_data (timestamp, value) VALUES (?, ?)" (1678886520 :: Int64, 11.8 :: Double)
  void $ execute conn "INSERT INTO timeseries_data (timestamp, value) VALUES (?, ?)" (1678886580 :: Int64, 15.1 :: Double)
  void $ execute conn "INSERT INTO timeseries_data (timestamp, value) VALUES (?, ?)" (1678886640 :: Int64, 13.7 :: Double)
