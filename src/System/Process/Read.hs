module System.Process.Read (
  Strng(..),
  readModifiedProcessWithExitCode,
  readModifiedProcess,
  readProcessWithExitCode,
  readProcess
  ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import GHC.IO.Exception (IOErrorType(OtherError, ResourceVanished), IOException(ioe_type))
import Prelude hiding (catch, null)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.IO hiding (hPutStr, hGetContents)
import qualified System.IO.Error as IO
import System.Process (CreateProcess(..), StdStream(CreatePipe, Inherit), proc, createProcess, waitForProcess, terminateProcess, showCommandForUser)

class Strng a where
  null :: a -> Bool
  hPutStr :: Handle -> a -> IO ()
  hGetContents :: Handle -> IO a

readProcessWithExitCode
    :: Strng a =>
       FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> a               -- ^ standard input
    -> IO (ExitCode, a, a) -- ^ exitcode, stdout, stderr
readProcessWithExitCode cmd args input =
  readModifiedProcessWithExitCode id cmd args input >>= \ (code, out, err, exn) ->
  maybe (return (code, out, err)) throw exn

-- | Like 'System.Process.readProcessWithExitCode', but using 'ByteString'
readModifiedProcessWithExitCode
    :: Strng a =>
       (CreateProcess -> CreateProcess)
                                -- ^ Modify CreateProcess with this
    -> FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> a               -- ^ standard input
    -> IO (ExitCode, a, a, Maybe IOError) -- ^ exitcode, stdout, stderr, exception
readModifiedProcessWithExitCode modify cmd args input = mask $ \restore -> do
    let modify' p = (modify p) {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }

    (Just inh, Just outh, Just errh, pid) <-
        createProcess (modify' (proc cmd args))

    flip onException
      (do hClose inh; hClose outh; hClose errh;
          terminateProcess pid; waitForProcess pid) $ restore $ do

      -- fork off a thread to start consuming stdout
      waitOut <- forkWait $ hGetContents outh

      -- fork off a thread to start consuming stderr
      waitErr <- forkWait $ hGetContents errh

      -- now write and flush any input
      exn <- if null input
             then return Nothing
             else (do hPutStr inh input >> hFlush inh >> hClose inh >> return Nothing) `catch` resourceVanished' (return . Just)

      -- wait on the output
      out <- waitOut
      err <- waitErr

      hClose outh
      hClose errh

      -- wait on the process
      ex <- waitForProcess pid

      return (ex, out, err, exn)

readProcess
    :: Strng a =>
       FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> a               -- ^ standard input
    -> IO a            -- ^ stdout
readProcess = readModifiedProcess id throw

-- | Like 'System.Process.readProcessWithExitCode', but using 'ByteString'
readModifiedProcess
    :: Strng a =>
       (CreateProcess -> CreateProcess)
                                -- ^ Modify CreateProcess with this
    -> (IOError -> IO ())       -- ^ What to on ResourceVanished exception - usually throw or const (return ())
    -> FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> a               -- ^ standard input
    -> IO a            -- ^ stdout
readModifiedProcess modify epipe cmd args input = mask $ \restore -> do
    let modify' p = (modify p) {std_in = CreatePipe, std_out = CreatePipe, std_err = Inherit }

    (Just inh, Just outh, _, pid) <-
        createProcess (modify' (proc cmd args))

    flip onException
      (do hClose inh; hClose outh;
          terminateProcess pid; waitForProcess pid) $ restore $ do

      -- fork off a thread to start consuming stdout
      waitOut <- forkWait $ hGetContents outh

      -- now write and flush any input
      unless (null input) $ (hPutStr inh input >> hFlush inh >> hClose inh) `catch` resourceVanished' epipe

      -- wait on the output
      out <- waitOut

      hClose outh

      -- wait on the process
      ex <- waitForProcess pid

      case ex of
        ExitSuccess   -> return out
        ExitFailure r -> ioError (mkError cmd args r)

forkWait :: IO a -> IO (IO a)
forkWait a = do
  res <- newEmptyMVar
  _ <- mask $ \restore -> forkIO $ try (restore a) >>= putMVar res
  return (takeMVar res >>= either (\ex -> throwIO (ex :: SomeException)) return)

-- | We tried to write the input and flush it, but the process exited.
-- Somebody else's problem, if it really is a problem.  Lots of programs
-- exit before they finish reading stdin.
resourceVanished' :: (IOError -> IO a) -> IOError -> IO a
resourceVanished' epipe e =
    if ioe_type e == ResourceVanished then epipe e else ioError e

mkError :: String -> [String] -> Int -> IOError
mkError cmd args r =
    IO.mkIOError OtherError ("readProcess: " ++ showCommandForUser cmd args ++
                                                             " (exit " ++ show r ++ ")")
                 Nothing Nothing
