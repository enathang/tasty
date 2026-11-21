-- | An abbreviated version of System.Timeout that uses weak references
module System.Timeout.Weak (timeout) where

import Control.Monad
import GHC.Event (getSystemTimerManager, registerTimeout, unregisterTimeout)
import Control.Concurrent
import Control.Exception (Exception(..), handleJust, bracket,
                          uninterruptibleMask_,
                          asyncExceptionToException,
                          asyncExceptionFromException)
import Data.Unique (Unique, newUnique)
import System.Mem.Weak (deRefWeak)

newtype Timeout = Timeout Unique deriving Eq

instance Show Timeout where
    show _ = "<<timeout>>"

instance Exception Timeout where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

timeout :: Int -> IO a -> IO (Maybe a)
timeout n f
    | n <  0    = fmap Just f
    | n == 0    = return Nothing
#if !defined(mingw32_HOST_OS) && !defined(javascript_HOST_ARCH)
    | rtsSupportsBoundThreads = do
        -- In the threaded RTS, we use the Timer Manager to delay the
        -- (fairly expensive) 'forkIO' call until the timeout has expired.
        --
        -- An additional thread is required for the actual delivery of
        -- the Timeout exception because killThread (or another throwTo)
        -- is the only way to reliably interrupt a throwTo in flight.
        pid <- myThreadId
        wpid <- mkWeakThreadId pid
        ex  <- fmap Timeout newUnique
        tm  <- getSystemTimerManager
        -- 'lock' synchronizes the timeout handler and the main thread:
        --  * the main thread can disable the handler by writing to 'lock';
        --  * the handler communicates the spawned thread's id through 'lock'.
        -- These two cases are mutually exclusive.
        lock <- newEmptyMVar
        let handleTimeout = do
                v <- isEmptyMVar lock
                when v $ void $ forkIOWithUnmask $ \unmask -> unmask $ do
                    v2 <- tryPutMVar lock =<< myThreadId
                    when v2 $ deRefWeak wpid >>= mapM_ (\t -> throwTo t ex)
            cleanupTimeout key = uninterruptibleMask_ $ do
                v <- tryPutMVar lock undefined
                if v then unregisterTimeout tm key
                     else takeMVar lock >>= killThread
        handleJust (\e -> if e == ex then Just () else Nothing)
                   (\_ -> return Nothing)
                   (bracket (registerTimeout tm n handleTimeout)
                            cleanupTimeout
                            (\_ -> fmap Just f))
#endif
    | otherwise = do
        pid <- myThreadId
        wpid <- mkWeakThreadId pid
        ex  <- fmap Timeout newUnique
        handleJust (\e -> if e == ex then Just () else Nothing)
                   (\_ -> return Nothing)
                   (bracket (forkIOWithUnmask $ \unmask ->
                                 unmask $ threadDelay n >> (deRefWeak wpid >>= mapM_ (\t -> throwTo t ex)))
                            (uninterruptibleMask_ . killThread)
                            (\_ -> fmap Just f))
        -- #7719 explains why we need uninterruptibleMask_ above.
