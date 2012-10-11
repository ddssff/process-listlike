module Utils where

import Control.Concurrent
import Control.Exception
import GHC.IO.Exception (IOErrorType(OtherError, ResourceVanished), IOException(ioe_type))
import qualified System.IO.Error as IO
import System.Process (showCommandForUser)

forkWait :: IO a -> IO (IO a)
forkWait a = do
  res <- newEmptyMVar
  _ <- mask $ \restore -> forkIO $ try (restore a) >>= putMVar res
  return (takeMVar res >>= either (\ex -> throwIO (ex :: SomeException)) return)

-- We tried to write the input and flush it, but the process exited.
-- Somebody else's problem, if it really is a problem.  Lots of programs
-- exit before they finish reading stdin.
resourceVanished :: IOError -> IO ()
resourceVanished e =
    if ioe_type e == ResourceVanished then return () else ioError e


mkError :: String -> [String] -> Int -> IOError
mkError cmd args r =
    IO.mkIOError OtherError ("readProcess: " ++ showCommandForUser cmd args ++
                                                             " (exit " ++ show r ++ ")")
                 Nothing Nothing
