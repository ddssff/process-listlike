-- | Versions of the functions in module 'System.Process.Read' specialized for type ByteString.
{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, TypeFamilies #-}
module System.Process.Read.Chars (
  Chars(..),
  readModifiedProcessWithExitCode,
  readModifiedProcess,
  readProcessWithExitCode,
  readProcess,
  ) where

import Control.Concurrent
import Control.Exception (SomeException, onException, evaluate, catch, try, throwIO, mask)
import Control.Monad
import GHC.IO.Exception (IOErrorType(OtherError, ResourceVanished), IOException(ioe_type))
import Prelude hiding (catch, null, length)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.IO hiding (hPutStr, hGetContents)
import qualified System.IO.Error as IO
import System.Process (CreateProcess(..), StdStream(CreatePipe, Inherit), proc,
                       CmdSpec(RawCommand, ShellCommand), showCommandForUser,
                       createProcess, waitForProcess, terminateProcess)

-- | Class of types which can be used as the input and outputs of the process functions.
class Integral (LengthType a) => Chars a where
  type LengthType a
  init :: a -> [Handle] -> IO ()
  -- ^ This should call 'hSetBinaryMode' on each handle if a is a
  -- ByteString type, so that it doesn't attempt to decode the text
  -- using the current locale.
  lazy :: a -> Bool
  length :: a -> LengthType a
  null :: a -> Bool
  hPutStr :: Handle -> a -> IO ()
  hGetContents :: Handle -> IO a

  append :: a -> a -> a
  concat :: [a] -> a
  toString :: a -> String
  fromString :: String -> a
  empty :: a

-- | A polymorphic implementation of
-- 'System.Process.readProcessWithExitCode' with a few
-- generalizations:
--
--    1. The input and outputs can be any instance of 'Chars'.
--
--    2. Allows you to modify the 'CreateProcess' record before the process starts
--
--    3. Takes a 'CmdSpec', so you can launch either a 'RawCommand' or a 'ShellCommand'.
readModifiedProcessWithExitCode
    :: forall a.
       Chars a =>
       CreateProcess   -- ^ process to run
    -> a               -- ^ standard input
    -> IO (ExitCode, a, a) -- ^ exitcode, stdout, stderr, exception
readModifiedProcessWithExitCode p input = mask $ \restore -> do
    (Just inh, Just outh, Just errh, pid) <-
        createProcess (p {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe })

    flip onException
      (do hClose inh; hClose outh; hClose errh;
          terminateProcess pid; waitForProcess pid) $ restore $ do
      (out, err) <- (if lazy input then readLazy else readStrict) inh outh errh

      hClose outh
      hClose errh

      -- wait on the process
      ex <- waitForProcess pid

      return (ex, out, err)
    where
      readLazy :: Handle -> Handle -> Handle -> IO (a, a)
      readLazy inh outh errh =
           do out <- hGetContents outh
              waitOut <- forkWait $ void $ force $ out
              err <- hGetContents errh
              waitErr <- forkWait $ void $ force $ err
              -- now write and flush any input
              writeInput inh
              -- wait on the output
              waitOut
              waitErr
              return (out, err)

      readStrict :: Handle -> Handle -> Handle -> IO (a, a)
      readStrict inh outh errh =
           do -- fork off a thread to start consuming stdout
              waitOut <- forkWait $ hGetContents outh
              -- fork off a thread to start consuming stderr
              waitErr <- forkWait $ hGetContents errh
              -- now write and flush any input
              writeInput inh
              -- wait on the output
              out <- waitOut
              err <- waitErr
              return (out, err)

      writeInput :: Handle -> IO ()
      writeInput inh = do
        (do unless (null input) (hPutStr inh input >> hFlush inh)
            hClose inh) `catch` resourceVanished (\ _ -> return ())

-- | A polymorphic implementation of
-- 'System.Process.readProcessWithExitCode' in terms of
-- 'readModifiedProcessWithExitCode'.
readProcessWithExitCode
    :: Chars a =>
       FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> a               -- ^ standard input
    -> IO (ExitCode, a, a) -- ^ exitcode, stdout, stderr
readProcessWithExitCode cmd args input = readModifiedProcessWithExitCode (proc cmd args) input

-- | Implementation of 'System.Process.readProcess' in terms of
-- 'readModifiedProcess'.
readProcess
    :: Chars a =>
       FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> a               -- ^ standard input
    -> IO a            -- ^ stdout
readProcess cmd args = readModifiedProcess (proc cmd args)

-- | A polymorphic implementation of 'System.Process.readProcess' with a few generalizations:
--
--    1. The input and outputs can be any instance of 'Chars'.
--
--    2. Allows you to modify the 'CreateProcess' record before the process starts
--
--    3. Takes a 'CmdSpec', so you can launch either a 'RawCommand' or a 'ShellCommand'.
readModifiedProcess
    :: Chars a =>
       CreateProcess   -- ^ process to run
    -> a               -- ^ standard input
    -> IO a            -- ^ stdout
readModifiedProcess p input = mask $ \restore -> do
    (Just inh, Just outh, _, pid) <-
        createProcess (p {std_in = CreatePipe, std_out = CreatePipe, std_err = Inherit })

    flip onException
      (do hClose inh; hClose outh;
          terminateProcess pid; waitForProcess pid) $ restore $ do
      out <- (if lazy input then readLazy else readStrict) inh outh

      hClose outh

      -- wait on the process
      ex <- waitForProcess pid

      case ex of
        ExitSuccess   -> return out
        ExitFailure r -> ioError (mkError "readModifiedProcess: " (cmdspec p) r)
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

      writeInput inh = do
         (do unless (null input) (hPutStr inh input >> hFlush inh)
             hClose inh) `catch` resourceVanished (\ _ -> return ())

forkWait :: IO a -> IO (IO a)
forkWait a = do
  res <- newEmptyMVar
  _ <- mask $ \restore -> forkIO $ try (restore a) >>= putMVar res
  return (takeMVar res >>= either (\ex -> throwIO (ex :: SomeException)) return)

-- | Wrapper for a process that provides a handler for the
-- ResourceVanished exception.  This is frequently an exception we
-- wish to ignore, because many processes will deliberately exit
-- before they have read all of their input.
resourceVanished :: (IOError -> IO a) -> IOError -> IO a
resourceVanished epipe e = if ioe_type e == ResourceVanished then epipe e else ioError e

-- | Create an exception for a process that exited abnormally.
mkError :: String -> CmdSpec -> Int -> IOError
mkError prefix (RawCommand cmd args) r =
    IO.mkIOError OtherError (prefix ++ showCommandForUser cmd args ++ " (exit " ++ show r ++ ")")
                 Nothing Nothing
mkError prefix (ShellCommand cmd) r =
    IO.mkIOError OtherError (prefix ++ cmd ++ " (exit " ++ show r ++ ")")
                 Nothing Nothing

force :: forall a. Chars a => a -> IO (LengthType a)
force x = evaluate $ length $ x
