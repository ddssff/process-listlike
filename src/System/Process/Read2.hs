-- | The hhGetContents function reads the output from two handles
-- (presumably stdout and stderr) and interleaves them into a list of
-- Output.  Unlike readProcessWithExitCode, this preserves the order
-- in which the chunks of text were written by the process.

{-# LANGUAGE ScopedTypeVariables #-}
module System.Process.Read2 (
  Strng2(..),
  Output(..),
  readProcessChunksWithExitCode,
  ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified GHC.IO.Exception as E
import Prelude hiding (catch, null, length, init)
import System.Exit (ExitCode)
import System.IO hiding (hPutStr, hGetContents)
import System.Process (CreateProcess(..), StdStream(CreatePipe),
                       CmdSpec, createProcess, waitForProcess, terminateProcess)
import System.Process.Read (Strng(init, null, hPutStr, length), proc', forkWait, resourceVanished)

-- | Class of types which can also be used by the hhGetContents function
class Strng a => Strng2 a where
  hGetNonBlocking :: Handle -> Int -> IO a

data Output a = Stdout a | Stderr a | Result ExitCode | Exception IOError deriving Show
data Readyness = Ready | Unready | EndOfFile

-- | This is a process runner which (at the cost of a busy loop) gives
-- you the chunks of text read from stdout and stderr interleaved in
-- the order they were written, along with any ResourceVanished
-- exxception that might occur.  Its interface is similar to
-- 'System.Process.Read.readModifiedProcessWithExitCode', though the
-- implementation is somewhat alarming.
readProcessChunksWithExitCode
    :: Strng2 a =>
       (CreateProcess -> CreateProcess)
                                -- ^ Modify CreateProcess with this
    -> CmdSpec                  -- ^ command to run
    -> a                        -- ^ standard input
    -> IO [Output a]
readProcessChunksWithExitCode modify cmd input = mask $ \restore -> do
    let modify' p = (modify p) {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }

    (Just inh, Just outh, Just errh, pid) <-
        createProcess (modify' (proc' cmd))

    init input [inh, outh, errh]

    flip onException
      (do hClose inh; hClose outh; hClose errh;
          terminateProcess pid; waitForProcess pid) $ restore $ do

      -- fork off one thread to start consuming both stdout and stderr
      waitOut <- forkWait $ hhGetContents outh errh

      -- now write and flush any input
      exns <- if null input
              then return []
              else (hPutStr inh input >> hFlush inh >> hClose inh >> return []) `catch` resourceVanished (return . (: []))

      -- wait on the output
      outs <- waitOut

      hClose outh
      hClose errh

      -- wait on the process
      ex <- waitForProcess pid

      return (outs ++ map Exception exns ++ [Result ex])

bufSize :: Int
bufSize = 4096		-- maximum chunk size
uSecs :: Int
uSecs = 8		-- minimum wait time, doubles each time nothing is ready
maxUSecs :: Int
maxUSecs = 100000	-- maximum wait time (microseconds)

-- | Read from two handles and interleave the output.
hhGetContents :: Strng2 a => Handle -> Handle -> IO [Output a]
hhGetContents outh errh = hhGetContents' (Just outh, Just errh, [])

hhGetContents' :: Strng2 a => (Maybe Handle, Maybe Handle, [Output a]) -> IO [Output a]
hhGetContents' (Nothing, Nothing, outputs) = return outputs
hhGetContents' (outh, errh, outputs) =
    hhGetContents'' uSecs >>= hhGetContents'
    where
      hhGetContents'' waitUSecs =
          do outReady <- maybe (return Unready) hReady' outh
             errReady <- maybe (return Unready) hReady' errh
             case (outReady, errReady) of
               (Unready, Unready) ->
                   do -- Neither output handle is ready, wait a bit.
                      -- Initial wait is 8 usecs, increasing to 0.1
                      -- sec.
                      threadDelay waitUSecs
                      hhGetContents'' (min maxUSecs (2 * waitUSecs))
               _ ->
                   do -- At least one handle is ready. Do a read.
                      (err, errh') <- nextOut errh errReady Stderr
                      (out, outh') <- nextOut outh outReady Stdout
                      return (outh', errh', outputs ++ err ++ out)

-- | Return the next output chunk and the updated handle from a handle.
nextOut :: Strng2 a => (Maybe Handle) -> Readyness -> (a -> Output a) -> IO ([Output a], Maybe Handle)
nextOut Nothing _ _ = return ([], Nothing)	-- Handle is closed
nextOut _ EndOfFile _ = return ([], Nothing)	-- Handle is closed
nextOut h Unready _ = return ([], h)	-- Handle is not ready
nextOut (Just h) Ready constructor =	-- Perform a read.
    do
      a <- hGetNonBlocking h bufSize
      case length a of
        -- A zero length read, unlike a zero length write, always
        -- means EOF.
        0 -> do hClose h
                return ([], Nothing)
        -- Got some input
        _n -> return ([constructor a], Just h)

-- | Determine whether a handle's state is Ready, Unready, or EndOfFile.
hReady' :: Handle -> IO Readyness
hReady' h =
  readyness `catch` handleEOF
  where
    readyness = hReady h >>= \ flag -> return $ if flag then Ready else Unready
    handleEOF e | E.ioe_type e == E.EOF = return EndOfFile
    handleEOF e = throw e
