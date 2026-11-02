-- | eBPF network-monitor collector. Thin FFI wrapper over the C shim in
-- @packets/@. The collector is optional: when it cannot start (no privileges,
-- no BTF, or built without eBPF support) it reports a message and the run
-- proceeds without it.
module Hh200.Ebpf
  ( Collector
  , startCollector
  , stopCollector
  ) where

import           Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, takeMVar, tryTakeMVar)
import           Data.Word (Word64)
import           Foreign.C.String (CString, peekCString)
import           Foreign.C.Types (CInt (..))
import           Foreign.Concurrent (newForeignPtr)
import           Foreign.ForeignPtr (ForeignPtr, finalizeForeignPtr, withForeignPtr)
import           Foreign.Marshal.Alloc (alloca)
import           Foreign.Ptr (Ptr, nullPtr)
import           Foreign.Storable (peek, poke)

-- | Opaque C handle (@struct hh_bpf@).
data HBpf

-- | A running collector: a libbpf handle, a polling thread, and its stop
-- signal.
data Collector = Collector
  { cHandle :: ForeignPtr HBpf
  , cStop   :: MVar ()
  , cDone   :: MVar ()
  }

foreign import ccall unsafe "hh_bpf_open"  c_hh_bpf_open  :: Ptr (Ptr HBpf) -> IO CInt
foreign import ccall safe   "hh_bpf_poll"  c_hh_bpf_poll  :: Ptr HBpf -> Ptr Word64 -> CInt -> IO CInt
foreign import ccall unsafe "hh_bpf_count" c_hh_bpf_count :: Ptr HBpf -> IO Word64
foreign import ccall safe   "hh_bpf_close" c_hh_bpf_close :: Ptr HBpf -> IO ()
foreign import ccall unsafe "hh_bpf_last_error" c_hh_bpf_last_error :: IO CString

-- | Try to start the collector. Returns 'Nothing' (after printing a clear
-- message) when eBPF is unavailable; the caller should simply proceed without
-- it.
startCollector :: IO (Maybe Collector)
startCollector = do
    mh <- alloca $ \pp -> do
        poke pp nullPtr
        r <- c_hh_bpf_open pp
        if r /= 0 then pure Nothing else Just <$> peek pp
    case mh of
        Nothing -> do
            err <- peekCString =<< c_hh_bpf_last_error
            putStrLn ("eBPF collector disabled: " ++ err)
            pure Nothing
        Just h -> do
            fptr <- newForeignPtr h (c_hh_bpf_close h)
            stop <- newEmptyMVar
            done <- newEmptyMVar
            _ <- forkIO (pollLoop h stop done)
            pure (Just (Collector fptr stop done))

-- | Stop the collector, return the final accumulated event count (e.g. TCP
-- retransmits), and release the handle.
stopCollector :: Collector -> IO Word64
stopCollector c = do
    putMVar (cStop c) ()
    takeMVar (cDone c)
    n <- withForeignPtr (cHandle c) c_hh_bpf_count
    finalizeForeignPtr (cHandle c)
    pure n

-- | Drain the ring buffer in 100 ms slices until signalled to stop. A @safe@
-- poll releases the RTS capability while blocked; the short timeout keeps the
-- thread responsive to the stop signal.
pollLoop :: Ptr HBpf -> MVar () -> MVar () -> IO ()
pollLoop h stop done = go
  where
    go = do
        r <- c_hh_bpf_poll h nullPtr 100
        stopped <- tryTakeMVar stop
        case stopped of
            Just () -> putMVar done ()
            Nothing
                | r < 0     -> putMVar done ()  -- poll error: stop the loop
                | otherwise -> go
