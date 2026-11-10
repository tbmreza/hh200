-- goal: subcommand generate writes latest-report.json
-- asserts xdg installed sqlite
{-# LANGUAGE DuplicateRecordFields #-}
-- {-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Hh200.Cli
  ( cli
  , mkArgs
  , go, Args(..)
  ) where

import Debug.Trace

import           Options.Applicative
import           Control.Exception (finally)
import           Control.Monad (unless)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad (forM_, replicateM, when, forever)
import           Control.Concurrent
import           Control.Concurrent.STM
import           System.Posix.Signals (installHandler, sigINT, Handler(CatchOnce))
import           System.Exit (exitWith, ExitCode(ExitFailure, ExitSuccess))
import           System.IO (hPutStrLn, stderr, stdout)
import qualified System.IO (hFlush)
import           System.Directory (getCurrentDirectory, doesFileExist)
import           Data.Aeson (ToJSON (..))
import qualified Data.Aeson as Json (encode)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text, pack)
import           Data.Version (showVersion)
import           Database.SQLite.Simple (Connection)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL

import           Network.Socket
import qualified Network.Socket.ByteString as NBS

import qualified Paths_hh200 (version)
import           Hh200.TokenBucketWorkerPool (RunState(..), courier)
import           Hh200.Types
import qualified Hh200.Scanner as Scanner
import           Hh200.Database (HtmlReportData, initDb, insertRun, RunRow(..), RREndTime(..))
import qualified Hh200.Database as AppDb
import           Hh200.LanguageServer (runTcp, runStdio)
import           Hh200.McpServer (runMcpStdio)
import           Hh200.Dashboard (startServer, controlSocketPath)


data Args = Args
  { mode ::        MainThread
  , source ::      Maybe String  -- used for both FilePath and Snippet sources
  , version ::     Bool
  , debugConfig :: Bool
  , call ::        Bool
  , nvu ::         Int
  , duration ::    Int
  , lsp ::         Maybe Int
  , lspStdio ::    Bool
  , mcpStdio ::    Bool
  } deriving (Show, Eq)

data MainThread =
    HttpLoad
  | Browse { browsePort :: Int }
  | Generate { generateOut :: Maybe FilePath }
  | Lsp { lspTcp :: Maybe Int }
  | Analyze { analyzeOpts :: AnalyzeOptions }
    deriving (Show, Eq)

-- | How `hh200 analyze` renders its result. Open to future formats (YAML, ...).
data OutputFormat =
    FmtText
  | FmtJson
    deriving (Show, Eq)

-- | Options for the analyze (static-check / config-resolution) mode. Kept open
-- so future use cases (snippet/stdin source, more formats, file output,
-- selecting config sources) are additive fields rather than new top-level flags.
data AnalyzeOptions = AnalyzeOptions
  { analyzeSource :: Maybe String   -- path; future: snippet, stdin
  , analyzeFormat :: OutputFormat
  , analyzeOut    :: Maybe FilePath -- where to write; Nothing = stdout
  } deriving (Show, Eq)


cli :: IO ()
cli = go =<< execParser inst

inst :: ParserInfo Args
inst = info (helper <*> (subcommands <|> mainCommand))
            (fullDesc <> header "Run hh200 scripts")
    where
    subcommands :: Parser Args
    subcommands = subparser
      ( command "browse"   (info (helper <*> subBrowse)   (progDesc "Launch the dashboard"))
     <> command "generate" (info (helper <*> subGenerate) (progDesc "Write an HTML report"))
     <> command "lsp"      (info (helper <*> subLsp)      (progDesc "Run the language server (stdio by default)"))
     <> command "analyze"  (info (helper <*> subAnalyze)  (progDesc "Resolve config / static-check without running side effects"))
      )

    subBrowse :: Parser Args
    subBrowse = setBrowseMode <$> portOption where
        setBrowseMode :: Int -> Args
        setBrowseMode p = mkArgs { mode = Browse { browsePort = p } }

        portOption :: Parser Int
        portOption = option auto
          ( long "port"
         <> short 'p'
         <> help "HTTP port for dashboard"
         <> value 8089
         <> showDefault
          )

    subGenerate :: Parser Args
    subGenerate = setGenerateMode <$> outOption where
        setGenerateMode :: Maybe FilePath -> Args
        setGenerateMode out = mkArgs { mode = Generate { generateOut = out } }

        outOption :: Parser (Maybe FilePath)
        outOption = optional $ strOption
          ( long "out"
         <> short 'o'
         <> metavar "PATH"
         <> help "Where to write the report (default: XDG data dir / latest-report.html)"
          )

    subLsp :: Parser Args
    subLsp = setLspMode <$> tcpOption where
        setLspMode :: Maybe Int -> Args
        setLspMode p = mkArgs { mode = Lsp { lspTcp = p } }

        tcpOption :: Parser (Maybe Int)
        tcpOption = optional $ option auto
          ( long "tcp"
         <> metavar "PORT"
         <> help "Serve the language server over TCP on 127.0.0.1:PORT (default: stdio)"
          )

    subAnalyze :: Parser Args
    subAnalyze = setAnalyzeMode <$> sourceArg <*> formatFlag <*> outOption where
        setAnalyzeMode :: Maybe String -> OutputFormat -> Maybe FilePath -> Args
        setAnalyzeMode src fmt out = mkArgs { mode = Analyze (AnalyzeOptions src fmt out) }

        sourceArg :: Parser (Maybe String)
        sourceArg = optional $ argument str
            (metavar "SOURCE" <> help "Path of source program")

        formatFlag :: Parser OutputFormat
        formatFlag = flag FmtText FmtJson
            (long "json" <> help "Emit the resolved config / analysis as JSON")

        outOption :: Parser (Maybe FilePath)
        outOption = optional $ strOption
          ( long "out"
         <> short 'o'
         <> metavar "PATH"
         <> help "Write output to PATH (default: stdout)"
          )

mainCommand :: Parser Args
mainCommand = build <$> sourceArgument
                    <*> versionFlag
                    <*> debugConfigFlag
                    <*> callFlag
                    <*> nvuOption
                    <*> durationOption
                    <*> lspPortOption
                    <*> lspStdioFlag
                    <*> mcpStdioFlag
    where
    build :: Maybe String -> Bool -> Bool -> Bool -> Int -> Int -> Maybe Int -> Bool -> Bool -> Args
    build src ver dbg cl n dur lspP lspS mcpS = mkArgs
      { source      = src
      , version     = ver
      , debugConfig = dbg
      , call        = cl
      , nvu         = n
      , duration    = dur
      , lsp         = lspP
      , lspStdio    = lspS
      , mcpStdio    = mcpS
      }

sourceArgument :: Parser (Maybe String)
sourceArgument = optional $ argument str
    (metavar "SOURCE" <> help "Path of source program")
 
versionFlag :: Parser Bool
versionFlag = switch
    (long "version" <> short 'V' <> help "Print version info and exit")

nvuOption :: Parser Int
nvuOption = option auto
  ( long "nvu"
 <> short 'n'
 <> metavar "N"
 <> help "Number of virtual users"
 <> value 1
 <> showDefault
  )
 
debugConfigFlag :: Parser Bool
debugConfigFlag = switch
  ( long "debug-config"
 <> short 'F'
 <> help "Read environment and script header to determine config values \
         \without executing the script's side-effects (deprecated: use `hh200 analyze`)"
  )
 
callFlag :: Parser Bool
callFlag = switch
    (long "call" <> short 'C' <> help "Execute a script snippet directly")
 
durationOption :: Parser Int
durationOption = option auto
  ( long "duration"
 <> short 't'
 <> metavar "S"
 <> help "Set duration of load test execution in seconds"
 <> value 0
 <> showDefault
  )
 
lspPortOption :: Parser (Maybe Int)
lspPortOption = optional $ option auto
    (long "lsp" <> short 'd' <> metavar "PORT" <> help "Run hh200 language server over TCP (deprecated: use `hh200 lsp --tcp PORT`)")

lspStdioFlag :: Parser Bool
lspStdioFlag = switch
    (long "lsp-stdio" <> help "Run hh200 language server over stdio (deprecated: use `hh200 lsp`)")

mcpStdioFlag :: Parser Bool
mcpStdioFlag = switch
    (long "mcp-stdio" <> help "Run hh200 MCP control server over stdio")

--         <*> optional ((\n a -> a { nvu = n, duration = 0 }) <$>
--                       option auto ( long "shotgun"
--                                  <> help "Alias for --nvu=N --duration=0"
--                                  <> metavar "N" ))

touch :: FilePath -> IO ()
touch = undefined

-- go and goMode indirection: debug programming directly in Script structs.
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

-- Run MCP control server over stdio.
-- hh200 --mcp-stdio
go Args { mcpStdio = True } = runMcpStdio

-- Run language server over stdio (the editor-facing default).
-- hh200 lsp
go Args { mode = Lsp { lspTcp = Nothing } } = runStdio

-- Run language server over TCP (debug/diagnostic affordance).
-- hh200 lsp --tcp 3000
go Args { mode = Lsp { lspTcp = Just port } } = runTcp port

-- Browse/dashboard mode.
-- hh200 browse
go Args { mode = Browse { browsePort } } = startServer (show browsePort)

-- 
-- hh200 generate
go Args { mode = Generate { generateOut } } = do
    conn <- initDb
    res <- AppDb.joinLatestReport conn

    out <- maybe getCurrentDirectory
                 pure
                 generateOut

    case res of
        Left _ -> do
            touch out
            exitWith (ExitFailure 1)
        Right Nothing ->
            -- info: paths ok, no data to generate report for
            touch out
        Right (Just (lr :: HtmlReportData)) -> do
            BL.writeFile out (Json.encode lr)

-- Analyze (static-check / config-resolution) mode.
-- hh200 analyze flow.hhs [--json] [--out PATH]
go Args { mode = Analyze { analyzeOpts } } = runAnalyze analyzeOpts

-- Deprecated alias: hh200 flow.hhs --debug-config
go Args { source = Just path, debugConfig = True } =
    runAnalyze (AnalyzeOptions (Just path) FmtText Nothing)

-- Script execution.
-- hh200 flow.hhs
-- go args@Args { duration = 1, call = False, rps = False, source = Just path } = do
go args@Args { duration = 1, call = False, source = Just path } = do
    exists <- doesFileExist path
    if not exists then do
        hPutStrLn stderr $ "error: file not found: " ++ path
        exitWith (ExitFailure 1)
    else do
        analyzed <- Scanner.analyze path
        case analyzed of
            Right script -> trace ("goMode path=" ++ show path) $ goMode script args
            Left err -> do
                hPutStrLn stderr ("error: " ++ err)
                exitWith (ExitFailure 1)

-- Inline program execution.
-- hh200 --call "GET http://localhost:9999/health"
go args@Args { call = True, source = Just snip } = do
    analyzed <- Scanner.analyze (Snippet (BL.fromStrict (C8.pack snip)))
    case analyzed of
        Right script -> goMode script args
        Left err -> do
            hPutStrLn stderr ("error: " ++ err)
            exitWith (ExitFailure 1)

-- Shotgun.
-- hh200 flow.hhs --nvu=4 --duration=0
        -- Just script  -> testShotgun n script
-- Load test mode.
-- hh200 flow.hhs --duration=30
go args@Args { duration = _n, call = False, source = Just path } = do
    mScript <- Scanner.analyze path
    case mScript of
        Left err -> do
            hPutStrLn stderr ("error: " ++ err)
            exitWith (ExitFailure 1)
        -- Right script  -> trace "this goMode!!!!" $ goMode script args
        Right script  -> goMode script args

-- Verifiable with `echo $?` which prints last exit code in shell.
go _ = exitWith (ExitFailure 1)

-- | Resolve config / static-check a script without executing its side effects.
-- Body is intentionally stubbed; the signature is the contract. Future work:
-- reuse `Scanner.analyze` + `Validate`, then render per 'OutputFormat'.
runAnalyze :: AnalyzeOptions -> IO ()
runAnalyze (AnalyzeOptions mSrc _fmt _out) =
    case mSrc of
        Nothing -> do
            hPutStrLn stderr "error: hh200 analyze requires a SOURCE path"
            exitWith (ExitFailure 1)
        Just path -> undefined path

genName :: Text
genName = "default run name"

goMode :: Script -> Args -> IO ()

goMode script args = do
    testSimple

    where
    -- terminate :: Flag -> IO ()
    terminate v = atomically $ writeTVar v Stopped

    mkRunRow :: IO RunRow
    mkRunRow = do
        pure $ RunRow
          { runId = 0
          , runControlSocket = "/tmp/uds_socket"
          , runStatus = "running"
          , runEndedAt = ETStillRunning
          -- args fields --
          , runRateLimit = 0.0
          , runScriptPath = pack $ fromMaybe "" (source args)
          -- system io --
          , runName = genName
          , runStartedAt = 0
          , runConcurrency = nvu args
          }

    -- Script of single call item, repeatedly fired for duration unless
    -- interrupted.
    testSimple :: IO ()
    testSimple = do
        conn <- initDb

        rr <- mkRunRow

        _mRunId <- insertRun conn rr

        -- Control flag.
        s <- newTVarIO Running

        -- The loop that fires HTTP requests.
        -- ??: Automatic 1-second stagger for nvu >= 4. Starting point could be
        -- to assume sequencing N imperative forkIO $ courier ... statements
        -- starts N couriers at the same instant.
        let argsNvu = nvu args
        drawer <- replicateM argsNvu newEmptyMVar
        -- forM_ (zip [] drawer) $ \(_, hole) -> do
        forM_ (drawer) $ \(hole) -> do
            forkIO $ courier Nothing script (s, duration args) hole

        -- Termination with ctrl+c, which is handled foremostly by worker.
        _ <- installHandler sigINT
                            (CatchOnce $ terminate s)
                            Nothing

        -- Unix Domain Socket listener
        _ <- forkIO $ do
            sockPath <- controlSocketPath
            -- controlSocketListener (runControlSocket rr) $ \msg ->  -- ??: xdg comply
            controlSocketListener sockPath $ \msg ->
                case msg of
                    "pause" ->  atomically $ writeTVar s Paused
                    "resume" -> atomically $ writeTVar s Running
                    "stop" ->   terminate s
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

        -- (auto)
        results <- mapM tryReadMVar drawer
        when (or [ b | Just b <- results ]) $
            exitWith (ExitFailure 1)


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
                            handler (C8.unpack bs)
                            loop
                 in loop)
                (close client)

        pure ()


-- -- persistMetrics :: Db.Connection -> TQueue LiveEvent -> Flag -> IO ()
-- -- persistMetrics conn e shutdownFlag = loop
-- persistMetrics :: Connection -> IO ()
-- persistMetrics conn = loop
-- -- flushTQueue :: TQueue a -> STM [a]
--     where
--     loop = do
--         case True of
--             True -> pure ()

-- Globally interruptible worker(s) running Script.
-- Worker(s) are dropped after the last CallItem.

mkArgs :: Args
mkArgs = Args { mode = HttpLoad
              , source = Nothing
              , version = False
              , debugConfig = False
              , call = False
              -- , rps = False
              , nvu = 1
              , duration = 0
              , lsp = Nothing
              , lspStdio = False
              , mcpStdio = False
              -- , browse = Nothing
              }
