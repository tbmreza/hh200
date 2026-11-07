{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Start and pre-flight ("plan") a load run from the dashboard.
--
-- The CLI currently owns run orchestration (see 'Hh200.Cli.goMode' /
-- @testSimple@). This module factors out just enough to launch a run
-- in-process so the dashboard's @POST \/api\/run@ can start one without
-- spawning a subprocess.
module Hh200.Runner
  ( RunRequest (..)
  , StartError (..)
  , runPlan
  , startRun
  ) where

import Debug.Trace

import           Control.Concurrent (forkIO, threadDelay, newEmptyMVar, takeMVar, putMVar, MVar)
import           Control.Concurrent.STM (atomically, newTVarIO, writeTVar, newTQueueIO, readTQueue, writeTQueue, TQueue)
import           Control.Monad (forM_, when, replicateM)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except (ExceptT (..), runExceptT, except, throwE)
import           Data.Aeson (Value, object, (.=), FromJSON (..), withObject, (.:))
import           Data.Int (Int64)
import           Data.List (isPrefixOf)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Database.SQLite.Simple (Connection)
import           System.Directory (canonicalizePath, doesFileExist, makeAbsolute)
import           System.Environment (lookupEnv)
import           System.FilePath ((</>), isRelative, normalise, splitDirectories)

import           Hh200.Database (initDb, insertRun, updateRunStatus, getLatestRunningRun, RunRow (..), RREndTime (..), insertCapturedRequest, CapturedRequest (..))
import           Hh200.Execution (CaptureCtx (..))
import           Hh200.Scanner (analyze, diagnostics)
import           Hh200.TokenBucketWorkerPool (courier, RunState (..))
import           Hh200.Types (Script (..))

-- | A start-run request body. @name@ may be blank; @script@ is a
-- server-side @.hhs@ path; @nvu@/ @duration@ mirror the CLI's @--nvu@ /
-- @--duration@.
data RunRequest = RunRequest
  { reqName     :: Text
  , reqScript   :: Text
  , reqNvu      :: Int
  , reqDuration :: Int
  } deriving (Show)

instance FromJSON RunRequest where
  parseJSON = withObject "RunRequest" $ \o ->
    RunRequest <$> o .: "name"
               <*> o .: "script"
               <*> o .: "nvu"
               <*> o .: "duration"

-- | Why a start request was rejected.
data StartError
  = StartConflict      -- ^ a run is already in progress
  | StartRejected Text -- ^ invalid input / script
  deriving (Show)

-- | Resolve a browser-supplied script path against the configured script root.
-- Rejects paths that escape the root (path traversal / arbitrary reads).
resolveScriptPath :: FilePath -> IO (Either Text FilePath)
resolveScriptPath raw = do
    mRoot <- lookupEnv "HH200_SCRIPT_ROOT"
    rootAbs <- makeAbsolute (fromMaybe "." mRoot) >>= canonicalizePath
    let joined = if isRelative raw then rootAbs </> raw else raw
    candAbs <- makeAbsolute joined >>= canonicalizePath
    let rootDirs = splitDirectories (normalise rootAbs)
        candDirs = splitDirectories (normalise candAbs)
    pure $
        if rootDirs `isPrefixOf` candDirs && length candDirs > length rootDirs
            then Right candAbs
            else Left (Text.pack ("script path escapes script root: " ++ raw))

-- | Pre-flight a run: validate the script and compute what we can foresee.
runPlan :: RunRequest -> IO Value
runPlan req
    | Text.null (reqScript req) = pure $ planInvalid [((1, 1) :: (Int, Int), "script path is required")]
    | otherwise = do
        mPath <- resolveScriptPath (Text.unpack (reqScript req))
        case mPath of
            Left e -> pure $ planInvalid [((1, 1) :: (Int, Int), Text.unpack e)]
            Right path -> do
                exists <- doesFileExist path
                if not exists
                    then pure $ planInvalid [((1, 1) :: (Int, Int), "file not found: " ++ path)]
                    else do
                        mScript <- analyze path
                        case mScript of
                            Left err -> do
                                content <- readFile path
                                diags <- diagnostics content
                                pure $ planInvalid (if null diags then [((1, 1) :: (Int, Int), err)] else diags)
                            Right script ->
                                pure $ planOk (reqNvu req) (reqDuration req) (length (callItems script))

planInvalid :: [((Int, Int), String)] -> Value
planInvalid diags = object
    [ "scriptValid" .= False
    , "errors" .= [ object [ "line" .= (l :: Int), "column" .= (c :: Int), "message" .= Text.pack m ]
                  | ((l, c), m) <- diags ]
    ]

planOk :: Int -> Int -> Int -> Value
planOk nvu duration nCallItems = object
    [ "scriptValid" .= True
    , "errors" .= ([] :: [Value])
    , "inFlightCeiling" .= max 0 nvu
    , "wallClock" .= wallClock
    , "rateLimit" .= object [ "known" .= False, "reason" .= rateReason ]
    , "totalRequests" .= totalRequests
    , "rateLimitSource" .= ("none" :: String)
    , "callItems" .= nCallItems
    ]
  where
    wallClock :: String
    wallClock = if duration <= 0 then "single-shot" else show duration ++ " seconds"

    rateReason :: String
    rateReason = "resolved from the script header/environment (--debug-config), not yet available in the UI"

    totalRequests :: Value
    totalRequests
        | duration <= 0 = object [ "known" .= True, "value" .= (max 0 nvu * nCallItems) ]
        | otherwise     = object [ "known" .= False, "reason" .= ("depends on the resolved rate limit" :: String) ]

-- | Magic values for a freshly-inserted run row.
defaultRunName :: Text
defaultRunName = "default run name"

defaultControlSocket :: Text
defaultControlSocket = "/tmp/uds_socket"

-- | Pure input validation, preserving the original checks and messages.
validateRequest :: RunRequest -> Either StartError ()
validateRequest req
    | Text.null (reqScript req) = Left (StartRejected "script path is required")
    | reqNvu req <= 0 = Left (StartRejected "nvu must be at least 1")
    | reqDuration req < 0 = Left (StartRejected "duration must be non-negative")
    | otherwise = Right ()

-- | Resolve the script path and parse the script, mapping failures to
-- 'StartRejected'.
resolveScript :: RunRequest -> ExceptT StartError IO (FilePath, Script)
resolveScript req = do
    path <- liftEither' StartRejected (resolveScriptPath (Text.unpack (reqScript req)))
    script <- liftEither' (StartRejected . Text.pack) (analyze path)
    pure (path, script)

-- | ??: specify the why and the when of ensuring no active runs.
ensureNoActiveRun :: Connection -> ExceptT StartError IO ()
ensureNoActiveRun conn = do
    active <- liftEither' StartRejected (getLatestRunningRun conn)
    case active of
        Just _  -> trace "AT:ensureNoActiveRun" $ throwE StartConflict
        Nothing -> pure ()

-- | Insert the @runs@ row and return its id.
insertNewRun :: Connection -> RunRequest -> FilePath -> ExceptT StartError IO Int64
insertNewRun conn req path =
    liftEither' StartRejected (insertRun conn (mkRunRow req path))

-- | Build the run row from the request, applying the default-name rule.
mkRunRow :: RunRequest -> FilePath -> RunRow
mkRunRow req path = RunRow
    { runId = 0
    , runName = if Text.null (reqName req) then defaultRunName else reqName req
    , runScriptPath = Text.pack path
    , runStartedAt = 0
    , runEndedAt = ETStillRunning
    , runStatus = "running"
    , runConcurrency = reqNvu req
    , runRateLimit = 0.0
    , runControlSocket = defaultControlSocket
    }

-- | Lift an @IO (Either e a)@ into the 'ExceptT' pipeline, mapping the error.
liftEither' :: (e -> StartError) -> IO (Either e a) -> ExceptT StartError IO a
liftEither' f action = ExceptT (fmap (either (Left . f) Right) action)

-- | Start a run: validate, insert a @runs@ row, then fork the couriers.
-- Returns the new run id. Rejects with 'StartConflict' when a run is already
-- in progress (best-effort: check-then-insert is not atomic).
startRun :: RunRequest -> IO (Either StartError Int64)
startRun req = runExceptT $ do
    except (validateRequest req)
    (path, script) <- resolveScript req
    conn <- liftIO initDb
    -- ensureNoActiveRun conn
    rid <- trace "AT:rid" $ insertNewRun conn req path
    liftIO $ launchWorkers conn rid script (reqNvu req) (reqDuration req)
    pure rid

-- | Fork one courier per virtual user, a duration timer (timed runs only),
-- and a completion watcher that marks the run @completed@/ @failed@.
launchWorkers :: Connection -> Int64 -> Script -> Int -> Int -> IO ()
launchWorkers conn runId script nvu duration = do
    cue <- newTVarIO Running
    drawer <- replicateM nvu newEmptyMVar
    queue <- newTQueueIO
    seqTv <- newTVarIO 0
    writerDone <- newEmptyMVar
    _ <- forkIO $ runWriter conn queue writerDone
    forM_ (zip [0 ..] drawer) $ \(workerId, hole) -> do
        let capCtx = CaptureCtx { ccQueue = queue, ccRunId = runId, ccWorkerId = workerId, ccSeq = seqTv }
        _ <- forkIO $ courier (Just capCtx) script (cue, duration) hole
        pure ()

    when (duration > 0) $ do
        _ <- forkIO $ threadDelay (duration * 1000000) >> atomically (writeTVar cue Stopped)
        pure ()

    _ <- forkIO $ do
        results <- mapM takeMVar drawer
        let failed = or results
        atomically $ writeTQueue queue Nothing  -- signal the writer to flush + stop
        takeMVar writerDone
        _ <- updateRunStatus conn runId (if failed then "failed" else "completed")
        pure ()

    pure ()

-- | Drains the capture queue into SQLite on a single thread (a @Connection@ is
-- not safe to share across threads). Terminates on the @Nothing@ sentinel.
runWriter :: Connection -> TQueue (Maybe CapturedRequest) -> MVar () -> IO ()
runWriter conn queue done = loop
  where
    loop = do
        mEv <- atomically (readTQueue queue)
        case mEv of
            Nothing -> putMVar done ()
            Just ev -> do
                _ <- insertCapturedRequest conn ev
                loop
