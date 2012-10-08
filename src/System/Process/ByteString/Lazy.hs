module System.Process.ByteString.Lazy where

import Control.Exception
import qualified Control.Exception as C (evaluate)
import Control.Monad
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import System.Process
import System.Exit (ExitCode)
import System.IO
import Utils (forkWait)

readProcessWithExitCode
    ::FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> ByteString               -- ^ standard input
    -> IO (ExitCode, ByteString, ByteString) -- ^ exitcode, stdout, stderr
readProcessWithExitCode = readModifiedProcessWithExitCode id

-- | Like 'System.Process.readProcessWithExitCode', but using 'ByteString'
readModifiedProcessWithExitCode
    :: (CreateProcess -> CreateProcess)
                                -- ^ Modify CreateProcess with this
    -> FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> ByteString               -- ^ standard input
    -> IO (ExitCode, ByteString, ByteString) -- ^ exitcode, stdout, stderr
readModifiedProcessWithExitCode modify cmd args input = mask $ \restore -> do
    let modify' p = modify (p {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe })

    (Just inh, Just outh, Just errh, pid) <-
        createProcess (modify' (proc cmd args))

    flip onException
      (do hClose inh; hClose outh; hClose errh;
            terminateProcess pid; waitForProcess pid) $ restore $ do

      -- fork off a thread to start consuming stdout
      out <- B.hGetContents outh
      waitOut <- forkWait $ void $ C.evaluate $ B.length out

      -- fork off a thread to start consuming stderr
      err <- B.hGetContents errh
      waitErr <- forkWait $ void $ C.evaluate $ B.length err

      -- now write and flush any input
      unless (B.null input) $ do B.hPutStr inh input; hFlush inh
      hClose inh -- done with stdin

      -- wait on the output
      waitOut
      waitErr

      hClose outh
      hClose errh

      -- wait on the process
      ex <- waitForProcess pid

      return (ex, out, err)

readProcess
    :: FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> ByteString               -- ^ standard input
    -> IO ByteString            -- ^ stdout
readProcess = readModifiedProcess id

-- | Like 'System.Process.readProcessWithExitCode', but using 'ByteString'
readModifiedProcess
    :: (CreateProcess -> CreateProcess)
                                -- ^ Modify CreateProcess with this
    -> FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> ByteString               -- ^ standard input
    -> IO ByteString            -- ^ stdout
readModifiedProcess modify cmd args input = mask $ \restore -> do
    let modify' p = modify (p {std_in = CreatePipe, std_out = CreatePipe, std_err = Inherit })

    (Just inh, Just outh, Just errh, pid) <-
        createProcess (modify' (proc cmd args))

    flip onException
      (do hClose inh; hClose outh; hClose errh;
            terminateProcess pid; waitForProcess pid) $ restore $ do

      -- fork off a thread to start consuming stdout
      out <- B.hGetContents outh
      waitOut <- forkWait $ void $ C.evaluate $ B.length out

      -- now write and flush any input
      unless (B.null input) $ do B.hPutStr inh input; hFlush inh
      hClose inh -- done with stdin

      -- wait on the output
      waitOut

      hClose outh

      -- wait on the process
      ex <- waitForProcess pid

      return out
