-- | The hhGetContents function reads the output from two handles
-- (presumably stdout and stderr) and interleaves them into a list of
-- Output.  Unlike readProcessWithExitCode, this preserves the order
-- in which the chunks of text were written by the process.

{-# LANGUAGE ScopedTypeVariables #-}
module System.Process.Read.Chunks (
  NonBlocking(..),
  Output(..),
  readProcessChunks
  ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified GHC.IO.Exception as E
import Prelude hiding (catch, null, length, init, rem)
import System.Exit (ExitCode)
import System.IO hiding (hPutStr, hGetContents)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Process (CreateProcess(..), StdStream(CreatePipe), ProcessHandle,
                       CmdSpec, createProcess, waitForProcess, terminateProcess)
import System.Process.Read.Chars (Chars(init, null, hPutStr, length), proc', forkWait, resourceVanished)

-- | Class of types which can also be used by 'System.Process.Read.readProcessChunks'.
class Chars a => NonBlocking a where
  hGetNonBlocking :: Handle -> Int -> IO a

data Output a = Stdout a | Stderr a | Result ExitCode | Exception IOError deriving Show

-- | This is a process runner which (at the cost of a busy loop) gives
-- you the chunks of text read from stdout and stderr interleaved in
-- the order they were written, along with any ResourceVanished
-- exxception that might occur.  Its interface is similar to
-- 'System.Process.Read.readModifiedProcessWith', though the
-- implementation is somewhat alarming.
readProcessChunks :: (NonBlocking a) =>
                     (CreateProcess -> CreateProcess)
                  -> CmdSpec
                  -> a
                  -> IO [Output a]
readProcessChunks modify cmd input = mask $ \ restore -> do
  let modify' p = (modify p) {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }

  (Just inh, Just outh, Just errh, pid) <-
      createProcess (modify' (proc' cmd))

  init input [inh, outh, errh]

  flip onException
    (do hClose inh; hClose outh; hClose errh;
        terminateProcess pid; waitForProcess pid) $ restore $ do

    waitOut <- forkWait $ elements pid (Just inh, Just outh, Just errh, [])

    -- now write and flush any input
    unless (null input) (hPutStr inh input >> hFlush inh >> hClose inh) `catch` resourceVanished (\ _e -> return ())

    -- wait on the output
    waitOut

-- | Take the info returned by 'createProcess' and gather and return
-- the stdout and stderr of the process.
elements :: NonBlocking a => ProcessHandle -> (Maybe Handle, Maybe Handle, Maybe Handle, [Output a]) -> IO [Output a]
elements pid (_, Nothing, Nothing, elems) =
    -- EOF on both output descriptors, get exit code.  It can be
    -- argued that the result will always contain exactly one exit
    -- code if traversed to its end, because the only case of
    -- 'elements' that does not recurse is the one that adds a Result,
    -- and there is nowhere else where a Result is added.  However,
    -- the process doing the traversing may die before that end is
    -- reached.
    do result <- waitForProcess pid
       -- Note that there is no need to insert the result code
       -- at the end of the list.
       return $ Result result : elems
elements pid tl@(_, _, _, []) =
    -- The available output has been processed, send input and read
    -- from the ready handles
    ready uSecs tl >>= elements pid
elements pid (inh, outh, errh, elems) =
    -- Add some output to the result value
    do etc <- unsafeInterleaveIO (elements pid (inh, outh, errh, []))
       return $ elems ++ etc

-- | Wait until at least one handle is ready and then write input or
-- read output.  Note that there is no way to check whether the input
-- handle is ready except to try to write to it and see if any bytes
-- are accepted.  If no input is accepted, or the input handle is
-- already closed, and none of the output descriptors are ready for
-- reading the function sleeps and tries again.
ready :: (NonBlocking a) =>
         Int -> (Maybe Handle, Maybe Handle, Maybe Handle, [Output a])
      -> IO (Maybe Handle, Maybe Handle, Maybe Handle, [Output a])
ready waitUSecs (inh, outh, errh, elems) =
    do
      outReady <- hReady' outh
      errReady <- hReady' errh
      case (inh, outReady, errReady) of
        -- Input handle closed and there are no ready output handles,
        -- wait a bit
        (Nothing, Unready, Unready) ->
            do threadDelay waitUSecs
               --ePut0 ("Slept " ++ show uSecs ++ " microseconds\n")
               ready (min maxUSecs (2 * waitUSecs)) (inh, outh, errh, elems)
        -- One or both output handles are ready, try to read from them
        _ ->
            do (out1, errh') <- nextOut errh errReady Stderr
               (out2, outh') <- nextOut outh outReady Stdout
               return (inh, outh', errh', elems ++ out1 ++ out2)

-- | Return the next output element and the updated handle
-- from a handle which is assumed ready.
nextOut :: NonBlocking a => Maybe Handle -> Readyness -> (a -> Output a) -> IO ([Output a], Maybe Handle)
nextOut Nothing _ _ = return ([], Nothing)	-- Handle is closed
nextOut _ EndOfFile _ = return ([], Nothing)	-- Handle is closed
nextOut h Unready _ = return ([], h)		-- Handle is not ready
nextOut (Just h) Ready constructor =	-- Perform a read
    do
      a <- hGetNonBlocking h bufSize
      case length a of
        -- A zero length read, unlike a zero length write, always
        -- means EOF.
        0 -> do hClose h
                return ([], Nothing)
        -- Got some output
        _n -> return ([constructor a], Just h)

data Readyness = Ready | Unready | EndOfFile

hReady' :: Maybe Handle -> IO Readyness
hReady' Nothing = return Unready
hReady' (Just h) = (hReady h >>= (\ flag -> return (if flag then Ready else Unready))) `catch` endOfFile (\ _ -> return EndOfFile)

endOfFile :: (IOError -> IO a) -> IOError -> IO a
endOfFile eeof e = if E.ioe_type e == E.EOF then eeof e else ioError e

bufSize :: Int
bufSize = 4096		-- maximum chunk size
uSecs :: Int
uSecs = 8		-- minimum wait time, doubles each time nothing is ready
maxUSecs :: Int
maxUSecs = 100000	-- maximum wait time (microseconds)
