-- | The hhGetContents function reads the output from two handles
-- (presumably stdout and stderr) and interleaves them into a list of
-- Output.  Unlike readProcessWithExitCode, this preserves the order
-- in which the chunks of text were written by the process.

{-# LANGUAGE ScopedTypeVariables #-}
module System.Process.Read.Chunks (
  NonBlocking(..),
  Output(..),
  foldOutput,
  foldOutputsL,
  foldOutputsR,
  readProcessChunks
  ) where

import Control.Applicative ((<$>))
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

-- | A fold function for the Output type, dispatches the value
-- depending on the constructor.
foldOutput :: Chars a =>
              (ExitCode -> b)
           -> (a -> b)
           -> (a -> b)
           -> (IOError -> b)
           -> Output a
           -> b
foldOutput codefn _ _ _ (Result code) = codefn code
foldOutput _ outfn _ _ (Stdout out) = outfn out
foldOutput _ _ errfn _ (Stderr err) = errfn err
foldOutput _ _ _ exnfn (Exception exn) = exnfn exn

foldOutputsL :: Chars a =>
                (b -> ExitCode -> b)
             -> (b -> a -> b)
             -> (b -> a -> b)
             -> (b -> IOError -> b)
             -> b
             -> [Output a]
             -> b
foldOutputsL _ _ _ _ result [] = result
foldOutputsL codefn outfn errfn exnfn result (x : xs) =
    let result' = foldOutput (codefn result) (outfn result) (errfn result) (exnfn result) x in
    foldOutputsL codefn outfn errfn exnfn result' xs
    -- Pretty sure this is equivalent:
    -- foldl (\ r x -> foldOutput (codefn r) (outfn r) (errfn r) (exnfn r) x) result xs

foldOutputsR :: forall a b. Chars a =>
                (b -> ExitCode -> b)
             -> (b -> a -> b)
             -> (b -> a -> b)
             -> (b -> IOError -> b)
             -> b
             -> [Output a]
             -> b
foldOutputsR _ _ _ _ result [] = result
foldOutputsR codefn outfn errfn exnfn result (x : xs) =
    let result' = foldOutputsR codefn outfn errfn exnfn result xs in
    foldOutput (codefn result') (outfn result') (errfn result') (exnfn result') x
    -- foldr (\ x r -> foldOutput (codefn r) (outfn r) (errfn r) (exnfn r) x) result xs

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

    waitOut <- forkWait $ elements pid (outh, errh)

    -- now write and flush any input
    unless (null input) (hPutStr inh input >> hFlush inh >> hClose inh) `catch` resourceVanished (\ _e -> return ())

    -- wait on the output
    waitOut

-- | Take the info returned by 'createProcess' and gather and return
-- the stdout and stderr of the process.
elements :: NonBlocking a => ProcessHandle -> (Handle, Handle) -> IO [Output a]
elements pid (outh, errh) =
    do outClosed <- hIsClosed outh
       errClosed <- hIsClosed errh
       case (outClosed, errClosed) of
         (True, True) ->
           -- EOF on both output descriptors, get exit code.  It can be
           -- argued that the result will always contain exactly one exit
           -- code if traversed to its end, because the only case of
           -- 'elements' that does not recurse is the one that adds a Result,
           -- and there is nowhere else where a Result is added.  However,
           -- the process doing the traversing may die before that end is
           -- reached.
           do result <- waitForProcess pid
              return [Result result]
         _ ->
           -- The available output has been processed, send input and read
           -- from the ready handles
           do elems' <- ready uSecs (outh, errh)
              case elems' of
                [] -> return []
                _ -> ((++) elems') <$> unsafeInterleaveIO (elements pid (outh, errh))

data Readyness = Ready | Unready | Closed

hReady' :: Handle -> IO Readyness
hReady' h =
    -- This check is necessary because if we run hReady on a closed
    -- handle it will keep giving us EndOfFile exceptions.
    hIsClosed h >>= checkReady
    where
      checkReady True = return Closed
      checkReady False =
          (hReady h >>= (\ flag -> return (if flag then Ready else Unready)))
            `catch` endOfFile (\ _ -> return Closed)

-- | Wait until at least one handle is ready and then read output.  If
-- none of the output descriptors are ready for reading the function
-- sleeps and tries again.
ready :: (NonBlocking a) =>
         Int -> (Handle, Handle)
      -> IO [Output a]
ready waitUSecs (outh, errh) =
    do
      outReady <- hReady' outh
      errReady <- hReady' errh
      case (outReady, errReady) of
        -- Input handle closed and there are no ready output handles,
        -- wait a bit
        (Unready, Unready) ->
            do threadDelay waitUSecs
               ready (min maxUSecs (2 * waitUSecs)) (outh, errh)
        _ ->
            -- One or both output handles are ready, try to read from
            -- them.  If (out1 ++ out2) is an empty list we know that
            -- we have reached EOF on both descriptors, because at
            -- least one was ready and nextOut only returns [] for a
            -- ready file descriptor on EOF.
            do out1 <- nextOut errh errReady Stderr
               out2 <- nextOut outh outReady Stdout
               return (out1 ++ out2)

-- | Return the next output element and the updated handle from a
-- handle which is assumed ready.  If the handle is closed or unready,
-- or we reach end of file an empty list of outputs is returned.
nextOut :: NonBlocking a => Handle -> Readyness -> (a -> Output a) -> IO [Output a]
nextOut h Ready constructor =	-- Perform a read
    do
      a <- hGetNonBlocking h bufSize
      case length a of
        -- A zero length read, unlike a zero length write, always
        -- means EOF.
        0 -> do hClose h
                return []
        -- Got some output
        _n -> return [constructor a]
nextOut _ _ _ = return []	-- Handle is closed or not ready

endOfFile :: (IOError -> IO a) -> IOError -> IO a
endOfFile eeof e = if E.ioe_type e == E.EOF then eeof e else ioError e

bufSize :: Int
bufSize = 4096		-- maximum chunk size
uSecs :: Int
uSecs = 8		-- minimum wait time, doubles each time nothing is ready
maxUSecs :: Int
maxUSecs = 100000	-- maximum wait time (microseconds)
