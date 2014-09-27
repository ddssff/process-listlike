{-# OPTIONS_GHC -Wall #-}
module Debug.Console
    ( console
    , ePutStr
    , ePutStrLn
    ) where

import Control.Monad.Trans (MonadIO, liftIO)
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Data.Monoid ((<>))
import System.IO (hPutStr, stderr, hFlush)
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE console #-}
console :: String -> IO ()
console = unsafePerformIO $ do
  v <- newEmptyMVar
  _tid <- forkIO (loop v)
  return (putMVar v)
    where loop v = takeMVar v >>= hPutStr stderr >> hFlush stderr >> loop v

ePutStr :: MonadIO m => String -> m ()
ePutStr s = liftIO $ console s
ePutStrLn :: MonadIO m => String -> m ()
ePutStrLn s = liftIO $ console (s <> "\n")
