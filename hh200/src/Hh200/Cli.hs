{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Hh200.Cli
  ( cli
  , go, Args(..), optsInfo
  , mkArgs
  ) where

import Debug.Trace

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Maybe
import           Control.Exception (finally, try, SomeException)
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TQueue (flushTQueue)
import           Control.Monad (forM_, replicateM, when, forever)
import           System.Posix.Signals (installHandler, sigINT, Handler(CatchOnce))
import           System.Exit (exitWith, ExitCode(ExitFailure))
import           System.IO (hPutStrLn, stderr, stdout)
import qualified System.IO (hFlush)
import           System.Directory (doesFileExist)
import           Web.Scotty (scotty, regex)
import qualified Web.Scotty as Server
import           Network.Wai.Middleware.Static (staticPolicy, addBase)
import           Options.Applicative
import           Data.Aeson (ToJSON (..), object, (.=))
import           Data.Int (Int64)
import           Data.Text (Text, pack)
import           Data.Version (showVersion)
import           Database.SQLite.Simple (FromRow (..), field, query_, Connection, execute, lastInsertRowId)
import qualified Paths_hh200 (version)

import Network.Socket
import qualified Network.Socket.ByteString as NBS
import Data.ByteString.Char8 (unpack)
import Control.Monad (unless)
import System.Directory (removeFile, doesFileExist)

import qualified Hh200.TokenBucketWorkerPool as Tbwp (wcWorkerId, wcRateLimiter, wcMode, WorkerConfig(..), worker, withRateLimiter, dummyDuo, WorkerMode(..))
import           Hh200.TokenBucketWorkerPool (RunState(..), worker, courier)
import           Hh200.Types
import qualified Hh200.Scanner as Scanner
import           Hh200.Database (initDb, closeDb)
import           Hh200.LanguageServer (runTcp, runStdio)


-- PICKUP
-- shotgun as shorthand for --nvu=N --duration=0
data Args = Args
  { source :: Maybe String  -- used for both FilePath and Snippet sources
  , version :: Bool
  , debugConfig :: Bool
  , call :: Bool
  , duration :: Int
  , lsp :: Maybe Int
  , lspStdio :: Bool
  , browse :: Maybe Int -- browse mode port (Nothing = not browse)
  } deriving (Show, Eq)

cli :: IO ()
cli = go =<< execParser optsInfo

optsInfo :: ParserInfo Args
-- ??: refactor so it's closer to natural language reading
optsInfo = info ((modeBrowse <|> modeA) <**> helper)
                (fullDesc <> header "Run hh200 scripts")
    where
    modeBrowse :: Parser Args
    modeBrowse = subparser $
        command "browse" $
            info (((\p -> mkArgs { browse = Just p }) <$>
                   option auto ( long "port"
                              <> short 'p'
                              <> help "HTTP port for dashboard"
                              <> value 8089
                              <> showDefault ))
                  <**> helper)
                 (progDesc "Launch the dashboard")

    modeA :: Parser Args
    modeA = Args
        -- Bound with Args fields by order, not by name; which makes sense as
        -- it allows different casing between above `debugConfig` and below
        -- `debug-config`.
        <$> optional (argument str (metavar "SOURCE"
                                 <> help "Path of source program"))

        <*> switch ( long "version"
                  <> short 'V'
                  <> help "Print version info and exit" )

        <*> switch ( long "debug-config"
                  <> short 'F'
                  <> help "Read environment and script header to determine the config values without executing script's side-effects" )

        <*> switch ( long "call"
                  <> short 'C'
                  <> help "Execute a script snippet directly" )

        -- <*> switch ( long "rps"
        --           <> short 'R'
        --           <> help "Start \"requests per second\" mode" )

        -- <*> option auto ( long "shotgun"
        --                <> short 'S'
        --                <> help "Execute in N parallel workers at once"
        --                <> metavar "N"       -- Displays as: -S N or --shotgun N
        --                <> value 1           -- Default to 1 if flag is omitted
        --                <> showDefault )     -- Shows "[default: 1]" in help text
        <*> option auto ( long "duration"
                       <> short 't'
                       <> help "Set duration of load test execution in seconds"
                       <> metavar "S"       -- Displays as: -t S or --duration S
                       <> value 0           -- Default 0 encodes shotgun mode
                       <> showDefault )     -- Shows "[default: 1]" in help text

        <*> optional ( option auto ( long "lsp"
                                   <> short 'd'
                                   <> help "Run hh200 language server"
                                   <> metavar "PORT" ) )

        <*> switch ( long "lsp-stdio"
                  <> help "Run hh200 language server over stdio" )

        <*> pure Nothing

-- go and goStraight indirection: debug programming directly in Script structs.
go :: Args -> IO ()

-- Print executable version.
-- hh200 --version
go Args { version = True } = do
    putStrLn $ showVersion Paths_hh200.version
    System.IO.hFlush stdout

-- Run language server.
-- hh200 --lsp=3000
go Args { lsp = Just port } = runTcp port

-- Run language server over stdio.
-- hh200 --lsp-stdio
go Args { lspStdio = True } = runStdio

-- Browse/dashboard mode.
-- hh200 browse
-- hh200 browse --port 9090
go Args { browse = Just port } = startServer (show port)

-- Static-check script.
-- hh200 flow.hhs --debug-config
go Args { source = Just path, debugConfig = True } = do
    let analyzed = Scanner.analyze path
    m <- runMaybeT analyzed
    case m of
        _ -> undefined
        -- ?? define debug-config

-- Script execution.
-- hh200 flow.hhs
-- ??: spin UDS listener here?
-- go args@Args { duration = 1, call = False, rps = False, source = Just path } = do
go args@Args { duration = 1, call = False, source = Just path } = do
    exists <- doesFileExist path
    if not exists then do
        hPutStrLn stderr $ "error: file not found: " ++ path
        exitWith (ExitFailure 1)
    else do
        let analyzed = Scanner.analyze path
        m <- runMaybeT analyzed
        case m of
            -- Just script -> trace ("path=" ++ show path) $ goStraight script args
            Just script -> trace ("goMode path=" ++ show path) $ goMode script args
            _ -> error "bug in hh200 grammar!"

-- Inline program execution.
-- hh200 --call "GET ..."
go Args { call = True, source = Just snip } =
    error ("undefined --call snip=" ++ snip)

-- -- Inserts timeseries data to a file database and optionally serves a web frontend.
-- -- hh200 flow.hhs --rps
-- go Args { rps = True, duration = n, source = Just path } = do
--     mScript <- runMaybeT (Scanner.analyze path)
--     case mScript of
--         Nothing -> exitWith (ExitFailure 1)
--         -- Just s  -> testRps rpsVal concurrency rampUpUs thinkTimeUs script
--         Just s  -> testRps 10 n 1000000 500000 s

-- Shotgun.
-- hh200 flow.hhs --nvu=4 --duration=0
        -- Just script  -> testShotgun n script
-- Load test mode.
-- hh200 flow.hhs --duration=30
go args@Args { duration = n, call = False, source = Just path } = do
    mScript <- runMaybeT (Scanner.analyze path)
    case mScript of
        Nothing -> exitWith (ExitFailure 1)
        Just script  -> goMode script args

-- Verifiable with `echo $?` which prints last exit code in shell.
go _ = exitWith (ExitFailure 1)

genName = "default run name"

-- data RunState = Running | Paused | Stopped
--     deriving (Eq)

-- data RateLimiter
--   = Unlimited
--   | TokenBucket
--       { tbTokens   :: TVar Int  -- current tokens
--       , tbCapacity :: Int       -- max tokens (burst ceiling)
--       , tbPerTick  :: Int       -- tokens added per refill tick
--       , tbInterval :: Int       -- refill interval in microseconds
--       }

-- staggerDelayMicr :: Int -> Int -> Int
-- staggerDelayMicr n i
--     | n >= 4    = i * (1_000_000 `div` n)   -- spread evenly across 1 s
--     | otherwise = 0

goMode :: Script -> Args -> IO ()

goMode script args = do
    testSimple

    where
    -- terminate :: Flag -> IO ()
    terminate v = atomically $ writeTVar v Stopped

    mkRunRow :: IO RunRow
    mkRunRow = do
        pure $ RunRow
          { runControlSocket = ""
          , runStatus = "running"
          , runEndedAt = ETStillRunning
          -- args fields --
          , runRateLimit = 0.0
          , runScriptPath = ""
          -- system io --
          , runName = genName
          , runStartedAt = 0
          }

    -- Script of single call item, repeatedly fired for duration unless
    -- interrupted.
    testSimple :: IO ()
    testSimple = do
        conn <- initDb

        rr <- mkRunRow

        mRunId <- insertRun conn rr

        -- Control flag.
        s <- newTVarIO Running

        -- The loop that fires HTTP requests. Automatic 1-second stagger for
        -- nvu >= 4.
        let argsNvu = 2
        drawer <- replicateM argsNvu newEmptyMVar
        -- forM_ (zip undefined drawer) $ \(_, hole) -> do
        forM_ (drawer) $ \(hole) -> do
            forkIO $ courier script s hole

        -- Termination with ctrl+c, which is handled foremostly by worker.
        _ <- installHandler sigINT
                            (CatchOnce $ terminate s)
                            Nothing

        -- Unix Domain Socket listener
        _ <- forkIO $
            controlSocketListener "/tmp/hh200_socket" $ \msg ->
                case msg of
                    "pause\n" -> undefined
                    "resume\n" -> atomically $ writeTVar s Running
                    "stop\n" -> terminate s
                    _        -> putStrLn $ "received: " ++ msg

        -- Termination when all workers are done.
        _ <- forkIO $ do
            forM_ drawer readMVar
            terminate s

        -- Termination based on timer only when duration > 0.
        when (duration args > 0) $ do
            _ <- forkIO $ do
                threadDelay ((duration args) * 1000000)
                terminate s
            pure ()

        atomically $
            check . (== Stopped) =<< readTVar s


controlSocketListener
    :: FilePath
    -> (String -> IO ())
    -> IO ()
controlSocketListener path handler = do
    sock <- socket AF_UNIX Stream defaultProtocol

    bind sock (SockAddrUnix path)
    listen sock 1

    forever $ do
        (client, _) <- accept sock

        _ <- forkIO $
            finally
                (let loop = do
                        bs <- NBS.recv client 4096
                        unless (bs == mempty) $ do
                            handler (unpack bs)
                            loop
                 in loop)
                (close client)

        pure ()


-- ??: what's a "live event"?
-- persistMetrics :: Db.Connection -> TQueue LiveEvent -> Flag -> IO ()
-- persistMetrics conn e shutdownFlag = loop
persistMetrics :: Connection -> IO ()
persistMetrics conn = loop
-- flushTQueue :: TQueue a -> STM [a]
    where
    loop = do
        case True of
            True -> pure ()

insertRun :: Connection -> RunRow -> IO (Maybe Int64)
insertRun conn rr = do
    result <- try $ do
        execute conn
                "INSERT INTO runs (name, script_path, started_at, ended_at, status, concurrency, rate_limit, control_socket) VALUES (?, ?, unixepoch('now'), ?, ?, ?, ?, ?)"
                ("dummy" :: Text, "test.hhs" :: Text, 0 :: Int64, "completed" :: Text, 1 :: Int, 0.0 :: Double, "" :: Text)
        lastInsertRowId conn
    case result of
        Left (_ :: SomeException) -> pure Nothing
        Right rid -> pure (Just rid)

-- goStraight :: Script -> Args -> IO ()
--
-- -- Script execution.
-- -- hh200 flow.hhs
-- -- - Always touches filesystem sqlite. Non-RPS runs are saved or not saved to database.
-- goStraight script args = do
--     testSimple
--
--     where
--     testSimple :: IO ()
--     testSimple = do
--         -- let scripts = workOptimize script
--         let scripts = Tbwp.dummyDuo script
--
--
--         conn <- initDb
--         let rr = RunRow
--                 { runName = "default"
--                 -- , runScriptPath = maybe "" pack args.source
--                 , runScriptPath = ""
--                 , runStartedAt = 0
--                 , runEndedAt = ETStillRunning
--                 , runStatus = "running"
--                 , runConcurrency = length scripts
--                 , runRateLimit = 0.0
--                 , runControlSocket = ""
--                 }
--         mRunId <- insertRun conn rr
--
--         shutdownFlag <- newTVarIO False
--         doneSignals <- replicateM (length scripts) newEmptyMVar
--
--         forM_ (zip3 [1..] scripts doneSignals) $ \(i, s, done) -> do
--             let cfg = Tbwp.WorkerConfig { Tbwp.wcMode = Tbwp.OneShot
--                                         , Tbwp.wcRateLimiter = Nothing
--                                         , Tbwp.wcWorkerId = i
--                                         }
--             forkIO (Tbwp.worker cfg s shutdownFlag done)
--
--         -- ??: (callsite of persistMetrics) when there's no live events left.
--         -- Termination with ctrl+c, which is handled foremostly by worker.
--         _ <- installHandler sigINT
--                             (CatchOnce (atomically $ writeTVar shutdownFlag True))
--                             Nothing
--
--         -- Termination when all workers are done.
--         _ <- forkIO $ do
--             forM_ doneSignals readMVar
--             atomically $ writeTVar shutdownFlag True
--
--         -- Termination based on timer.
--         _ <- forkIO $ do
--             threadDelay (10 * 1000000)
--             atomically $ writeTVar shutdownFlag True
--
--         atomically (readTVar shutdownFlag >>= check)
--         -- closeDb conn  -- ??:


-- Globally interruptible worker(s) running Script.
-- Worker(s) are dropped after the last CallItem.

-- Concurrent one-shot: fire N workers, report how many failed.
testShotgun :: Int -> Script -> IO ()
testShotgun numWorkers script = do
    shutdownFlag <- newTVarIO False
    doneSignals <- replicateM numWorkers newEmptyMVar

    forM_ (zip [1..numWorkers] doneSignals) $ \(i, done) -> do
        let cfg = Tbwp.WorkerConfig { Tbwp.wcMode = Tbwp.OneShot, Tbwp.wcRateLimiter = Nothing, Tbwp.wcWorkerId = i }
        forkIO (Tbwp.worker cfg script shutdownFlag done)

    -- Termination with ctrl+c.
    _ <- installHandler sigINT
                        (CatchOnce (atomically $ writeTVar shutdownFlag True))
                        Nothing  -- Other signals to block.

    -- Termination based on timer.
    _ <- forkIO $ do
        threadDelay (10 * 1000000)
        atomically $ writeTVar shutdownFlag True

    -- Wait for all workers to finish.
    forM_ doneSignals takeMVar
    putStrLn $ "# testShotgun: " ++ show numWorkers ++ " workers completed."

-- Rampup-able pool of virtual users with rate limiting.
-- RPS: rate of individual CallItems.
-- ??: if the name testRps survives, move to where clause of goStraight
testRps :: Int -> Int -> Int -> Int -> Script -> IO ()
testRps rpsVal concurrency rampUpUs thinkTimeUs script = do
    -- connect "timeseries.db"

    shutdownFlag <- newTVarIO False
    doneSignals <- replicateM concurrency newEmptyMVar

    -- Tbwp.withRateLimiter (Tbwp.RateLimiterConfig rpsVal rpsVal) $ \rl -> do
    Tbwp.withRateLimiter (undefined rpsVal rpsVal) $ \rl -> do
        -- Ramp-up: fork one VU at a time with delay between each.
        forM_ (zip [1..concurrency] doneSignals) $ \(i, done) -> do
            let cfg = Tbwp.WorkerConfig
                    { Tbwp.wcMode = Tbwp.LoopWithNap thinkTimeUs
                    , Tbwp.wcRateLimiter = Just rl
                    , Tbwp.wcWorkerId = i
                    }
            _ <- forkIO (Tbwp.worker cfg script shutdownFlag done)
            when (i < concurrency) $ threadDelay rampUpUs

        putStrLn $ "# testRps: rate=" ++ show rpsVal ++ " reqs/sec, workers=" ++ show concurrency

        -- Termination with ctrl+c.
        _ <- installHandler sigINT
                            (CatchOnce (atomically $ writeTVar shutdownFlag True))
                            Nothing  -- Other signals to block.

        -- Termination based on timer.
        _ <- forkIO $ do
            threadDelay (10 * 1000000)
            atomically $ writeTVar shutdownFlag True

        atomically (readTVar shutdownFlag >>= check)

mkArgs :: Args
mkArgs = Args { source = Nothing
              , version = False
              , debugConfig = False
              , call = False
              -- , rps = False
              , duration = 1
              , lsp = Nothing
              , lspStdio = False
              , browse = Nothing
              }

--------------------------------------------------------------------------------
-- Dashboard server
--------------------------------------------------------------------------------
-- Path                  Notes
-- GET /api/runs         List runs from SQLite
-- GET /api/runs/live    In-progress run at the time being
-- GET /api/runs/:runId  Completed run detail
-- GET /sse              SSE stream for live inserts
-- POST /sig             Receives pause/resume/stop from dashboard, forwards to UDS
-- GET /*                SvelteKit static SPA

-- ?? full review of RunRow: runEndedAt is optional
data RREndTime =
    ETStillRunning
  | ETHasEnded !Int64

-- ??: use the same source of truth between sequelize and haskell toJSON interface
data RunRow = RunRow
  -- { runName          :: !Text
  -- , runScriptPath    :: !Text
  -- , runStartedAt     :: Int64
  -- , runEndedAt       :: RREndTime
  -- , runStatus        :: !Text
  -- , runConcurrency   :: !Int
  -- , runRateLimit     :: !Double
  -- , runControlSocket :: !Text
  -- }
  { runName          :: Text
  , runScriptPath    :: Text
  , runStartedAt     :: Int64
  , runEndedAt       :: RREndTime
  , runStatus        :: Text
  , runConcurrency   :: Int
  , runRateLimit     :: Double
  , runControlSocket :: Text
  }

instance FromRow RunRow where
    fromRow = RunRow <$> field
                     <*> field
                     <*> field
                     <*> (maybe ETStillRunning ETHasEnded <$> field)
                     <*> field
                     <*> field
                     <*> field
                     <*> field

instance ToJSON RunRow where
    toJSON (RunRow n sp sa ea s c rl cs) = object
      [ "name" .= n
      , "script_path" .= sp
      , "started_at" .= sa
      , "ended_at" .= case ea of ETStillRunning -> Nothing; ETHasEnded v -> Just v
      , "status" .= s
      , "concurrency" .= c
      , "rate_limit" .= rl
      , "control_socket" .= cs
      ]

startServer :: String -> IO ()
startServer portStr = do
    let port = read portStr :: Int
    putStrLn $ "hh200 dashboard listening on http://localhost:" ++ show port
    scotty port $ do
        Server.middleware (staticPolicy (addBase "min"))

        Server.get "/runs" $ do
            conn <- liftIO initDb
            -- (auto)
            rows <- liftIO $ (query_ conn "SELECT name, script_path, started_at, ended_at, status, concurrency, rate_limit, control_socket FROM runs ORDER BY started_at DESC" :: IO [RunRow])
            Server.json $ object ["runs" .= rows]

        Server.get (regex "(.*)") $
            Server.file "min/200.html"

        Server.post "/sig" $
            -- pause/resume warrants ack; stop is fire-and-forget.
            -- ??: write to an ipc
            undefined
