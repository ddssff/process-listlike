-- | Versions of the functions in module 'System.Process.Read' specialized for type ByteString.
{-# LANGUAGE ScopedTypeVariables #-}
module System.Process.Read (
  Strng(..),
  readModifiedProcessWithExitCode,
  readModifiedProcess,
  readProcessWithExitCode,
  readProcess,
  ignoreResourceVanished,
  proc',
  forkWait,
  resourceVanished
  ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Int (Int64)
import GHC.IO.Exception (IOErrorType(OtherError, ResourceVanished), IOException(ioe_type))
import Prelude hiding (catch, null, length)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.IO hiding (hPutStr, hGetContents)
import qualified System.IO.Error as IO
import System.Process (CreateProcess(..), StdStream(CreatePipe, Inherit), proc, shell,
                       CmdSpec(RawCommand, ShellCommand), showCommandForUser,
                       createProcess, waitForProcess, terminateProcess)

-- | Class of types which can be used as the input and outputs of the process functions.
class Strng a where
  lazy :: a -> Bool
  length :: a -> Int64
  null :: a -> Bool
  hPutStr :: Handle -> a -> IO ()
  hGetContents :: Handle -> IO a

force :: forall a. Strng a => a -> IO Int64
force x = evaluate $ length $ x

-- | A version of 'System.Process.readProcessWithExitCode' with a few generalizations:
--
--    1. The input and outputs can be any instance of Strng.
--
--    2. Allows you to modify the CreateProcess record before the process starts
--
--    3. Returns any ResourceVanished exception that occurs as the fourth tuple element
--
--    4. Takes a CmdSpec, so you can launch either a RawCommand or a ShellCommand.
readModifiedProcessWithExitCode
    :: forall a.
       Strng a =>
       (CreateProcess -> CreateProcess)
                                -- ^ Modify CreateProcess with this
    -> CmdSpec                  -- ^ command to run
    -> a               -- ^ standard input
    -> IO (ExitCode, a, a, Maybe IOError) -- ^ exitcode, stdout, stderr, exception
readModifiedProcessWithExitCode modify cmd input = mask $ \restore -> do
    let modify' p = (modify p) {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }

    (Just inh, Just outh, Just errh, pid) <-
        createProcess (modify' (proc' cmd))

    flip onException
      (do hClose inh; hClose outh; hClose errh;
          terminateProcess pid; waitForProcess pid) $ restore $ do
      (out, err, exn) <- (if lazy input then readLazy else readStrict) inh outh errh

      hClose outh
      hClose errh

      -- wait on the process
      ex <- waitForProcess pid

      return (ex, out, err, exn)
    where
      readLazy :: Handle -> Handle -> Handle -> IO (a, a, Maybe IOError)
      readLazy inh outh errh =
           do out :: a <- hGetContents outh
              waitOut <- forkWait $ void $ force $ out
              err <- hGetContents errh
              waitErr <- forkWait $ void $ force $ out
              -- now write and flush any input
              exn <- writeInput inh
              -- wait on the output
              waitOut
              waitErr
              return (out, err, exn)

      readStrict :: Handle -> Handle -> Handle -> IO (a, a, Maybe IOError)
      readStrict inh outh errh =
           do -- fork off a thread to start consuming stdout
              waitOut <- forkWait $ hGetContents outh
              -- fork off a thread to start consuming stderr
              waitErr <- forkWait $ hGetContents errh
              -- now write and flush any input
              exn <- writeInput inh
              -- wait on the output
              out <- waitOut
              err <- waitErr
              return (out, err, exn)

      writeInput :: Handle -> IO (Maybe IOError)
      writeInput inh =
          if null input
          then return Nothing
          else (do hPutStr inh input
                   hFlush inh
                   hClose inh
                   return Nothing) `catch` resourceVanished (return . Just)


-- | An implementation of 'System.Process.readProcessWithExitCode' in terms
-- of 'readModifiedProcessWithExitCode'.  Fails on ResourceVanished exception.
readProcessWithExitCode
    :: Strng a =>
       FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> a               -- ^ standard input
    -> IO (ExitCode, a, a) -- ^ exitcode, stdout, stderr
readProcessWithExitCode cmd args input =
  readModifiedProcessWithExitCode id (RawCommand cmd args) input >>= \ (code, out, err, exn) ->
  maybe (return (code, out, err)) throw exn

-- | Implementation of 'System.Process.readProcess' in terms of
-- 'readModifiedProcess'.  Fails on ResourceVanished exception.
readProcess
    :: Strng a =>
       FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> a               -- ^ standard input
    -> IO a            -- ^ stdout
readProcess cmd args = readModifiedProcess id throw (RawCommand cmd args)

-- | A version of 'System.Process.readProcess' with a few generalizations:
--
--    1. The input and outputs can be any instance of Strng.
--
--    2. Allows you to modify the CreateProcess record before the process starts
--
--    3. Has a handler for ResourceVanished exceptions
--
--    4. Takes a CmdSpec, so you can launch either a RawCommand or a ShellCommand.
readModifiedProcess
    :: Strng a =>
       (CreateProcess -> CreateProcess)
                                -- ^ Modify CreateProcess with this
    -> (IOError -> IO ())       -- ^ What to on ResourceVanished exception - usually 'throw' or 'ignoreResourceVanished'
    -> CmdSpec                  -- ^ command to run
    -> a               -- ^ standard input
    -> IO a            -- ^ stdout
readModifiedProcess modify epipe cmd input = mask $ \restore -> do
    let modify' p = (modify p) {std_in = CreatePipe, std_out = CreatePipe, std_err = Inherit }

    (Just inh, Just outh, _, pid) <-
        createProcess (modify' (proc' cmd))

    flip onException
      (do hClose inh; hClose outh;
          terminateProcess pid; waitForProcess pid) $ restore $ do
      out <- (if lazy input then readLazy else readStrict) inh outh

      hClose outh

      -- wait on the process
      ex <- waitForProcess pid

      case ex of
        ExitSuccess   -> return out
        ExitFailure r -> ioError (mkError cmd r)
    where
      readLazy inh outh =
          do -- fork off a thread to start consuming stdout
             out <- hGetContents outh
             waitOut <- forkWait $ void $ force $ out
             writeInput inh
             waitOut
             return out

      readStrict inh outh =
          do waitOut <- forkWait $ hGetContents outh
             writeInput inh
             waitOut

      writeInput inh =
         unless (null input) (do hPutStr inh input
                                 hFlush inh
                                 hClose inh) `catch` resourceVanished epipe

forkWait :: IO a -> IO (IO a)
forkWait a = do
  res <- newEmptyMVar
  _ <- mask $ \restore -> forkIO $ try (restore a) >>= putMVar res
  return (takeMVar res >>= either (\ex -> throwIO (ex :: SomeException)) return)

-- | Wrapper for a process that provides a handler for the
-- ResourceVanished exception.  This is frequently an exception we
-- wish to ignore, because many processes will exit before they have
-- read all of their input.
resourceVanished :: (IOError -> IO a) -> IOError -> IO a
resourceVanished epipe e = if ioe_type e == ResourceVanished then epipe e else ioError e

-- | Exception handler that ignores ResourceVanished. 
ignoreResourceVanished :: IOError -> IO ()
ignoreResourceVanished = resourceVanished ignore

-- | Create an exception for a process that exited abnormally.
mkError :: CmdSpec -> Int -> IOError
mkError (RawCommand cmd args) r =
    IO.mkIOError OtherError ("readProcess: " ++ showCommandForUser cmd args ++ " (exit " ++ show r ++ ")")
                 Nothing Nothing
mkError (ShellCommand cmd) r =
    IO.mkIOError OtherError ("readProcess: " ++ cmd ++ " (exit " ++ show r ++ ")")
                 Nothing Nothing

ignore :: IOError -> IO ()
ignore _ = return ()

proc' :: CmdSpec -> CreateProcess
proc' (RawCommand cmd args) = proc cmd args
proc' (ShellCommand cmd) = shell cmd
