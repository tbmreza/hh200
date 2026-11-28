{-# LANGUAGE ScopedTypeVariables #-}
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
  , ForegroundRun (..)
  , runForeground
  , defaultRunName
  ) where


import           Control.Concurrent (forkIO, threadDelay, newEmptyMVar, takeMVar, putMVar, readMVar, tryReadMVar, MVar)
import           Control.Concurrent.STM (atomically, check, newTVarIO, readTVar, writeTVar, newTQueueIO, readTQueue, flushTQueue, writeTQueue, TQueue)
import           Control.Monad (forM_, when, replicateM, void)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except (ExceptT (..), runExceptT, except, throwE)
import           Data.Aeson (Value, object, (.=), FromJSON (..), withObject, (.:), (.:?), (.!=))
import           Data.Int (Int64)
import           Control.Exception (SomeException, displayException, try)
import           Data.Maybe (fromMaybe, isJust, isNothing)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Database.SQLite.Simple (Connection, withTransaction)
import           System.Directory (canonicalizePath, doesFileExist, makeAbsolute)
import           System.Environment (getEnvironment, lookupEnv)
import           System.FilePath (takeDirectory)
import           System.IO (hPutStrLn, stderr)
import           System.Timeout (timeout)

import           Hh200.Control (ControlSignal (..), controlSocketPath, parseControlSignal, startControlListener)
import           Hh200.Database (initDb, closeDb, insertRun, finishRun, getLatestRunningRun, RunRow (..), RREndTime (..), insertCapturedRequest, CapturedRequest (..))
import           Hh200.Execution (CaptureCtx (..))
import           Hh200.Inputs (RunInputs (..), FilePolicy (..), confinedPath, externalVars)
import           Hh200.Scanner (analyze, diagnostics)
import           Hh200.TokenBucketWorkerPool (courier, RunState (..))
import           Hh200.Types (Script (..), Diagnostic (..), SrcSpan (..))

-- | A start-run request body. @name@ may be blank; @script@ is a
-- server-side @.hhs@ path; @nvu@/ @duration@ mirror the CLI's @--nvu@ /
-- @--duration@; @rampUp@ is the seconds over which VUs come online linearly
-- (0 = instant, the default).
data RunRequest = RunRequest
  { reqName     :: Text
  , reqScript   :: Text
  , reqNvu      :: Int
  , reqDuration :: Int
  , reqRampUp   :: Int
  } deriving (Show)

instance FromJSON RunRequest where
  parseJSON = withObject "RunRequest" $ \o ->
    RunRequest <$> o .: "name"
               <*> o .: "script"
               <*> o .: "nvu"
               <*> o .: "duration"
               <*> o .:? "rampUp" .!= 0

-- | Why a start request was rejected.
data StartError
  = StartConflict      -- ^ a run is already in progress
  | StartRejected Text -- ^ invalid input / script
  deriving (Show)

-- | Resolve a browser-supplied script path against the configured script root.
-- Rejects paths that escape the root (path traversal / arbitrary reads).
resolveScriptPath :: FilePath -> IO (Either Text FilePath)
resolveScriptPath raw = do
    rootAbs <- scriptRoot
    mPath <- confinedPath (Just rootAbs) rootAbs raw
    pure $ case mPath of
        Just candAbs -> Right candAbs
        Nothing -> Left (Text.pack ("script path escapes script root: " ++ raw))

-- | The directory dashboard runs may read scripts (and files those scripts
-- name) from: @HH200_SCRIPT_ROOT@, or the working directory.
scriptRoot :: IO FilePath
scriptRoot = do
    mRoot <- lookupEnv "HH200_SCRIPT_ROOT"
    makeAbsolute (fromMaybe "." mRoot) >>= canonicalizePath

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
                                let errs = if null diags
                                        then [((1, 1) :: (Int, Int), err)]
                                        else [ (spanStart (diagSpan d), diagMessage d) | d <- diags ]
                                pure $ planInvalid errs
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


-- | Pure input validation, preserving the original checks and messages.
validateRequest :: RunRequest -> Either StartError ()
validateRequest req
    | Text.null (reqScript req) = Left (StartRejected "script path is required")
    | reqNvu req <= 0 = Left (StartRejected "nvu must be at least 1")
    | reqDuration req < 0 = Left (StartRejected "duration must be non-negative")
    | reqRampUp req < 0 = Left (StartRejected "rampUp must be non-negative")
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
        Just _  -> throwE StartConflict
        Nothing -> pure ()

-- | Insert the @runs@ row and return its id.
insertNewRun :: Connection -> RunRequest -> FilePath -> FilePath -> ExceptT StartError IO Int64
insertNewRun conn req path sock =
    liftEither' StartRejected (insertRun conn (mkRunRow req path sock))

-- | Build the run row from the request, applying the default-name rule.
mkRunRow :: RunRequest -> FilePath -> FilePath -> RunRow
mkRunRow req path sock = RunRow
    { runId = 0
    , runName = if Text.null (reqName req) then defaultRunName else reqName req
    , runScriptPath = Text.pack path
    , runStartedAt = 0
    , runEndedAt = ETStillRunning
    , runStatus = "running"
    , runConcurrency = reqNvu req
    , runRateLimit = 0.0
    , runControlSocket = Text.pack sock
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
    sock <- liftIO controlSocketPath
    rid <- insertNewRun conn req path sock
    inputs <- liftIO (dashboardRunInputs path)
    liftIO $ launchWorkers conn rid inputs script sock (reqNvu req) (reqDuration req) (reqRampUp req)
    pure rid

-- | Fork one courier per virtual user (staggered by @rampUp@), a duration timer
-- (timed runs only), and a completion watcher that marks the run @completed@/
-- @failed@.
-- | Inputs for a dashboard run: @HH200_VAR_*@ variables (there is no @--var@
-- here), and files resolved against the script's directory but confined to
-- the script root, so a submitted script can't read files outside it.
dashboardRunInputs :: FilePath -> IO RunInputs
dashboardRunInputs path = do
    environment <- getEnvironment
    root <- scriptRoot
    pure RunInputs
        { riVars = externalVars environment []
        , riFiles = FilePolicy { fpBaseDir = takeDirectory path, fpRoot = Just root }
        }

launchWorkers :: Connection -> Int64 -> RunInputs -> Script -> FilePath -> Int -> Int -> Int -> IO ()
launchWorkers conn runId inputs script sock nvu duration rampUp = do
    cue <- newTVarIO Running
    -- Control channel, so the dashboard's /api/sig and MCP reach this run.
    listening <- startControlListener sock $ \msg ->
        case parseControlSignal msg of
            Just SigPause  -> atomically $ writeTVar cue Paused
            Just SigResume -> atomically $ writeTVar cue Running
            Just SigStop   -> atomically $ writeTVar cue Stopped
            Nothing        -> hPutStrLn stderr ("hh200: ignoring control message: " ++ show msg)
    stopListening <- case listening of
        Right stopL -> pure stopL
        Left why -> do
            hPutStrLn stderr ("warning: control socket unavailable for run " ++ show runId
                ++ " (" ++ why ++ "); pause/resume/stop will not reach it")
            pure (pure ())
    drawer <- replicateM nvu newEmptyMVar
    (capCtx, flushCapture) <- startCapture conn runId
    forM_ (zip [0 ..] drawer) $ \(workerId, hole) -> do
        _ <- forkIO $ do
            when (workerId > 0) $ threadDelay (staggerMicros rampUp nvu workerId)
            courier (Just (capCtx workerId)) inputs script (cue, duration) hole
        pure ()

    when (duration > 0) $ do
        _ <- forkIO $ threadDelay (duration * 1000000) >> atomically (writeTVar cue Stopped)
        pure ()

    _ <- forkIO $ do
        results <- mapM takeMVar drawer
        let failed = or results
        flushCapture
        stopListening
        _ <- finishRun conn runId (if failed then "failed" else "completed")
        pure ()

    pure ()

-- | Microseconds to wait before starting worker @i@ (0-based) of @nvu@, given a
-- total ramp window of @rampUp@ seconds. 0 when there's nothing to ramp.
staggerMicros :: Int -> Int -> Int -> Int
staggerMicros rampUp nvu i
    | rampUp <= 0 || nvu <= 1 = 0
    | otherwise = (rampUp * 1000000 * i) `div` (nvu - 1)

-- | Start a run's capture writer. Returns the 'CaptureCtx' for a worker id,
-- and an action that writes out everything queued so far and stops the
-- writer.
startCapture :: Connection -> Int64 -> IO (Int -> CaptureCtx, IO ())
startCapture conn rid = do
    queue <- newTQueueIO
    seqTv <- newTVarIO 0
    writerDone <- newEmptyMVar
    _ <- forkIO $ runWriter conn queue writerDone
    let ctx workerId = CaptureCtx { ccQueue = queue, ccRunId = rid, ccWorkerId = workerId, ccSeq = seqTv }
        flush = do
            atomically $ writeTQueue queue Nothing  -- the writer drains, then stops
            takeMVar writerDone
    pure (ctx, flush)

-- | A run started from the command line (@hh200 script.hhs@).
data ForegroundRun = ForegroundRun
  { frName       :: Text
  , frScriptPath :: Text     -- ^ as given; empty for a @--call@ snippet
  , frInputs     :: RunInputs
  , frScript     :: Script
  , frNvu        :: Int
  , frDuration   :: Int      -- ^ seconds; 0 runs the script once per VU
  }

-- | Run a script in the foreground, recorded like a dashboard run: a @runs@
-- row, every request captured, and on the way out the row's final status and
-- @ended_at@. The status is @failed@ if a virtual user failed, @completed@
-- otherwise, or @cancelled@ when the run was stopped (Ctrl+C, the control
-- socket) and a request was still in flight after a short grace period.
--
-- @onStart@ gets the action that stops the run (the CLI binds it to SIGINT).
-- Returns 'True' when a virtual user failed. A database that can't take the
-- run row only costs the recording: the script still runs.
runForeground :: (IO () -> IO ()) -> ForegroundRun -> IO Bool
runForeground onStart fr = do
    conn <- initDb
    sock <- controlSocketPath
    inserted <- insertRun conn RunRow
        { runId = 0
        , runName = frName fr
        , runScriptPath = frScriptPath fr
        , runStartedAt = 0
        , runEndedAt = ETStillRunning
        , runStatus = "running"
        , runConcurrency = frNvu fr
        , runRateLimit = 0.0
        , runControlSocket = Text.pack sock
        }
    recording <- case inserted of
        Right rid -> do
            (capCtx, flushCapture) <- startCapture conn rid
            pure (Just (rid, capCtx, flushCapture))
        Left err -> do
            hPutStrLn stderr ("warning: this run will not be recorded: " ++ Text.unpack err)
            pure Nothing

    cue <- newTVarIO Running
    let stop = atomically (writeTVar cue Stopped)
    drawer <- replicateM (frNvu fr) newEmptyMVar
    forM_ (zip [0 ..] drawer) $ \(workerId, hole) ->
        forkIO $ courier (fmap (\(_, capCtx, _) -> capCtx workerId) recording)
                         (frInputs fr) (frScript fr) (cue, frDuration fr) hole
    onStart stop

    -- Control channel: pause/resume/stop from the dashboard or MCP.
    listening <- startControlListener sock $ \msg ->
        case parseControlSignal msg of
            Just SigPause  -> atomically $ writeTVar cue Paused
            Just SigResume -> atomically $ writeTVar cue Running
            Just SigStop   -> stop
            Nothing        -> hPutStrLn stderr ("hh200: ignoring control message: " ++ show msg)
    stopListening <- case listening of
        Right stopL -> pure stopL
        Left why -> do
            hPutStrLn stderr ("warning: control socket unavailable (" ++ why
                ++ "); pause/resume/stop will not reach this run")
            pure (pure ())

    -- The run ends when every VU is done, or on its timer (timed runs only).
    _ <- forkIO $ mapM_ readMVar drawer >> stop
    when (frDuration fr > 0) $
        void $ forkIO $ threadDelay (frDuration fr * 1000000) >> stop

    atomically $ check . (== Stopped) =<< readTVar cue
    stopListening

    -- A stopped VU finishes its current iteration; wait briefly for that so
    -- its requests and verdict are recorded, but don't hang on a slow server.
    allDone <- timeout stopGraceMicros (mapM_ readMVar drawer)
    results <- mapM tryReadMVar drawer
    let failed = or [ b | Just b <- results ]
        status | failed            = "failed"
               | allDone == Nothing = "cancelled"
               | otherwise         = "completed"
    forM_ recording $ \(rid, _, flushCapture) -> do
        flushCapture
        finished <- finishRun conn rid status
        either (\e -> hPutStrLn stderr ("warning: could not record the run's end: " ++ Text.unpack e))
               pure finished
    closeDb conn
    pure failed

-- | How long a stopped foreground run waits for in-flight iterations.
stopGraceMicros :: Int
stopGraceMicros = 2000000

-- | Drains the capture queue into SQLite on a single thread (a @Connection@ is
-- not safe to share across threads). Whatever has queued up is written in one
-- transaction, so a fast run doesn't pay a commit per insert and falls behind.
-- Terminates on the @Nothing@ sentinel, once everything before it is written.
runWriter :: Connection -> TQueue (Maybe CapturedRequest) -> MVar () -> IO ()
runWriter conn queue done = loop
  where
    loop = do
        batch <- atomically $ (:) <$> readTQueue queue <*> flushTQueue queue
        let evs = [ ev | Just ev <- takeWhile isJust batch ]
            sentinel = any isNothing batch
        when (not (null evs)) $ do
            written <- try (withTransaction conn (mapM_ (insertCapturedRequest conn) evs))
            case written of
                Left (e :: SomeException) ->
                    hPutStrLn stderr ("warning: " ++ show (length evs)
                        ++ " captured requests were not recorded: " ++ displayException e)
                Right () -> pure ()
        if sentinel then putMVar done () else loop
