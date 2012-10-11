{-# LANGUAGE ScopedTypeVariables #-}
-- | Variations of the readProcess and readProcessWithExitCode functions
-- from System.Process which read and write ByteStrings and have an
-- extra argument to modify the CreateProcess value before the process
-- is started.
module System.Process.ByteString.Lazy where

import Control.Exception (evaluate, mask, onException, catch)
import Control.Monad (void, unless)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Utils (forkWait, resourceVanished, mkError)
import Prelude hiding (catch)
import System.Exit (ExitCode(..))
import System.IO (hFlush, hClose)
import System.Process (CreateProcess(std_in, std_out, std_err), createProcess, waitForProcess, proc,
                       StdStream(CreatePipe, Inherit), terminateProcess)

-- | Like 'System.Process.readProcessWithExitCode', but using 'ByteString'
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
    let modify' p = (modify p) {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }

    (Just inh, Just outh, Just errh, pid) <-
        createProcess (modify' (proc cmd args))

    flip onException
      (do hClose inh; hClose outh; hClose errh;
            terminateProcess pid; waitForProcess pid) $ restore $ do

      -- fork off a thread to start consuming stdout
      out <- B.hGetContents outh
      waitOut <- forkWait $ void $ evaluate $ B.length out

      -- fork off a thread to start consuming stderr
      err <- B.hGetContents errh
      waitErr <- forkWait $ void $ evaluate $ B.length err

      -- now write and flush any input
      unless (B.null input) $ (do B.hPutStr inh input
                                  hFlush inh
                                  hClose inh) `catch` resourceVanished

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

readModifiedProcess
    :: (CreateProcess -> CreateProcess)
                                -- ^ Modify CreateProcess with this
    -> FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> ByteString               -- ^ standard input
    -> IO ByteString            -- ^ stdout
readModifiedProcess modify cmd args input = mask $ \restore -> do
    let modify' p = modify (p {std_in = CreatePipe, std_out = CreatePipe, std_err = Inherit })

    (Just inh, Just outh, _, pid) <-
        createProcess (modify' (proc cmd args))

    flip onException
      (do hClose inh; hClose outh;
            terminateProcess pid; waitForProcess pid) $ restore $ do

      -- fork off a thread to start consuming stdout
      out <- B.hGetContents outh
      waitOut <- forkWait $ void $ evaluate $ B.length out

      -- now write and flush any input
      unless (B.null input) $ (do B.hPutStr inh input
                                  hFlush inh
                                  hClose inh) `catch` resourceVanished

      -- wait on the output
      waitOut

      -- wait on the process
      ex <- waitForProcess pid

      case ex of
        ExitSuccess   -> return out
        ExitFailure r -> ioError (mkError cmd args r)
