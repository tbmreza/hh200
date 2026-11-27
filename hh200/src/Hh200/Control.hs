{-# LANGUAGE ScopedTypeVariables #-}

-- | The run control channel: a Unix domain socket that receives the words
-- @pause@, @resume@ and @stop@.
--
-- A running load test listens on it ('startControlListener'); the dashboard's
-- @POST \/api\/sig@ and the MCP server's control tools write to it
-- ('controlSocketWriter'). Everyone resolves the path with 'controlSocketPath'.
module Hh200.Control
  ( controlSocketPath
  , controlSocketWriter
  , ControlSignal (..)
  , parseControlSignal
  , startControlListener
  ) where

import           Control.Concurrent (forkIO, killThread)
import           Control.Exception (IOException, SomeException, bracketOnError, finally, try)
import           Control.Monad (forever, unless, void)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import           Data.Char (isSpace)
import           Data.Maybe (fromMaybe)
import           Network.Socket
import qualified Network.Socket.ByteString as NBS
import qualified Network.Socket.ByteString.Lazy as NBSL
import           System.Environment (lookupEnv)
import           System.Posix.Files (fileExist, getSymbolicLinkStatus, isSocket, removeLink)

-- | Path of the Unix domain socket that receives pause/resume/stop signals.
-- Overridable via @HH200_CONTROL_SOCKET@ so tests can point it at a socket
-- they own.
controlSocketPath :: IO FilePath
controlSocketPath = fromMaybe "/tmp/uds_socket" <$> lookupEnv "HH200_CONTROL_SOCKET"

-- | Send one message to the run listening on @path@. Throws an 'IOException'
-- when nothing is listening.
controlSocketWriter :: FilePath -> BL.ByteString -> IO ()
controlSocketWriter path msg =
    bracketOnError (socket AF_UNIX Stream defaultProtocol) close $ \sock -> do
        connect sock (SockAddrUnix path)
        NBSL.sendAll sock msg
        close sock

data ControlSignal = SigPause | SigResume | SigStop
    deriving (Show, Eq)

-- | Recognise a control message, ignoring surrounding whitespace so that
-- @echo stop | nc -U \/tmp\/uds_socket@ works too.
parseControlSignal :: String -> Maybe ControlSignal
parseControlSignal raw =
    case trim raw of
        "pause"  -> Just SigPause
        "resume" -> Just SigResume
        "stop"   -> Just SigStop
        _        -> Nothing
  where
    trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

-- | Listen on @path@ and hand every received message to @handler@.
--
-- A socket file left behind by an earlier run is removed first. If another
-- process is still accepting on @path@, or @path@ is something other than a
-- socket, nothing is touched and the reason comes back as 'Left'.
--
-- On success the result is the action that stops listening and removes the
-- socket file; call it when the run ends.
startControlListener :: FilePath -> (String -> IO ()) -> IO (Either String (IO ()))
startControlListener path handler = do
    cleared <- clearStaleSocket path
    case cleared of
        Left why -> pure (Left why)
        Right () -> do
            r <- try (bracketOnError (socket AF_UNIX Stream defaultProtocol) close $ \sock -> do
                    bind sock (SockAddrUnix path)
                    listen sock 8
                    pure sock)
            case r of
                Left (e :: IOException) -> pure (Left (show e))
                Right sock -> do
                    tid <- forkIO (acceptLoop sock)
                    pure (Right (stop tid sock))
  where
    acceptLoop sock = forever $ do
        (client, _) <- accept sock
        void $ forkIO $ readAll client C8.empty `finally` close client

    -- One message per connection: read to EOF, then dispatch.
    readAll client acc = do
        chunk <- NBS.recv client 4096
        if C8.null chunk
            then unless (C8.null acc) (handler (C8.unpack acc))
            else readAll client (acc <> chunk)

    stop tid sock = do
        killThread tid
        close sock
        void (try (removeLink path) :: IO (Either IOException ()))

-- | Make @path@ free to bind: remove a dead socket file, refuse to touch a
-- live one or a non-socket.
clearStaleSocket :: FilePath -> IO (Either String ())
clearStaleSocket path = do
    exists <- fileExist path
    if not exists
        then pure (Right ())
        else do
            st <- getSymbolicLinkStatus path
            if not (isSocket st)
                then pure (Left (path ++ " exists and is not a socket"))
                else do
                    live <- try (controlSocketWriter path BL.empty)
                    case live of
                        Right () -> pure (Left ("another hh200 run is already listening on " ++ path))
                        Left (_ :: SomeException) -> do
                            r <- try (removeLink path)
                            pure $ case r of
                                Right () -> Right ()
                                Left (e :: IOException) -> Left ("cannot remove stale " ++ path ++ ": " ++ show e)
