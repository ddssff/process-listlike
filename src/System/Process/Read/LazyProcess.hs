{-# LANGUAGE PackageImports, ScopedTypeVariables, TypeFamilies #-}
{-# OPTIONS -Wwarn -Wall -fno-warn-name-shadowing -fno-warn-missing-signatures #-}
-- | Function to run a process and return a lazy list of chunks from
--   standard output, standard error, and at the end of the list an
--   object indicating the process result code.  If neither output
--   handle is ready for reading the process sleeps and tries again,
--   with the sleep intervals increasing from 8 microseconds to a
--   maximum of 0.1 seconds.
module System.Unix.Progress.LazyProcess
    ( lazyRun
    , lazyProcess
    ) where

import Control.Concurrent (threadDelay)
import "mtl" Control.Monad.Trans (MonadIO(liftIO))
import Control.Exception
import qualified Data.ByteString.Lazy as L
import qualified GHC.IO.Exception as E
import Prelude hiding (catch, init, null, length)
import System.IO (Handle, hReady, hClose, hFlush)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Process (ProcessHandle, waitForProcess, terminateProcess,
                       CmdSpec, CreateProcess(..), StdStream(CreatePipe), createProcess)
import System.Process.Read ()
import System.Process.Read.Chars (proc', Chars(init, null, length, hPutStr), forkWait, resourceVanished)
import System.Process.Read.Chunks (NonBlocking(hGetNonBlocking), Output(..))

-- |An opaque type would give us additional type safety to ensure the
-- semantics of 'exitCodeOnly'.
bufSize :: Int
bufSize = 4096		-- maximum chunk size
uSecs :: Int
uSecs = 8		-- minimum wait time, doubles each time nothing is ready
maxUSecs :: Int
maxUSecs = 100000	-- maximum wait time (microseconds)

-- | Create a process with 'runInteractiveProcess' and run it with 'lazyRun'.
lazyProcess :: (NonBlocking a, a ~ L.ByteString) =>
               (CreateProcess -> CreateProcess)
            -> CmdSpec
            -> L.ByteString
            -> IO [Output a]
lazyProcess modify cmd input = mask $ \ restore -> do
  let modify' p = (modify p) {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }

  (Just inh, Just outh, Just errh, pid) <-
      createProcess (modify' (proc' cmd))

  init input [inh, outh, errh]

  flip onException
    (do hClose inh; hClose outh; hClose errh;
        terminateProcess pid; waitForProcess pid) $ restore $ do

    waitOut <- forkWait $ lazyRun (Just inh, Just outh, Just errh, pid)

    -- now write and flush any input
    _exns <- if null input
            then return []
            else (hPutStr inh input >> hFlush inh >> hClose inh >> return []) `catch` resourceVanished (return . (: []))

    -- wait on the output
    waitOut

  -- liftIO (runInteractiveProcess exec args cwd env) >>= lazyRun input

-- | Take a tuple like that returned by 'runInteractiveProcess',
-- create a process, send the list of inputs to its stdin and return
-- the lazy list of 'Output' objects.
lazyRun :: forall m a. (MonadIO m, NonBlocking a, a ~ L.ByteString) =>
           (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> m [Output a]
lazyRun (inh, outh, errh, pid) = liftIO $
    elements (inh, outh, errh, [])
    where
      elements :: (Maybe Handle, Maybe Handle, Maybe Handle, [Output a]) -> IO [Output a]
      -- EOF on both output descriptors, get exit code.  It can be
      -- argued that the list will always contain exactly one exit
      -- code if traversed to its end, because the only case of
      -- elements that does not recurse is the one that adds a Result,
      -- and there is nowhere else where a Result is added.  However,
      -- the process doing the traversing may die before that end is
      -- reached.
      elements (_, Nothing, Nothing, elems) =
          do result <- waitForProcess pid
             -- Note that there is no need to insert the result code
             -- at the end of the list.
             return $ Result result : elems
      -- The available output has been processed, send input and read
      -- from the ready handles
      elements tl@(_, _, _, []) = ready uSecs tl >>= elements
      -- Add some output to the result value
      elements (inh, outh, errh, elems) =
          do
            etc <- unsafeInterleaveIO (elements (inh, outh, errh, []))
            return $ elems ++ etc

-- A quick fix for the issue where hWaitForInput has actually started
-- raising the isEOFError exception in ghc 6.10.
data Readyness = Ready | Unready | EndOfFile

hReady' :: Handle -> IO Readyness
hReady' h = (hReady h >>= (\ flag -> return (if flag then Ready else Unready))) `catch` (\ (e :: IOError) ->
                                                                                             case E.ioe_type e of
                                                                                               E.EOF -> return EndOfFile
                                                                                               _ -> error (show e))

-- | Wait until at least one handle is ready and then write input or
-- read output.  Note that there is no way to check whether the input
-- handle is ready except to try to write to it and see if any bytes
-- are accepted.  If no input is accepted, or the input handle is
-- already closed, and none of the output descriptors are ready for
-- reading the function sleeps and tries again.
ready :: (NonBlocking a, a ~ L.ByteString) =>
         Int -> (Maybe Handle, Maybe Handle, Maybe Handle, [Output a])
      -> IO (Maybe Handle, Maybe Handle, Maybe Handle, [Output a])
ready waitUSecs (inh, outh, errh, elems) =
    do
      outReady <- maybe (return Unready) hReady' outh
      errReady <- maybe (return Unready) hReady' errh
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
nextOut :: (a ~ L.ByteString) =>
           (Maybe Handle) -> Readyness -> (a -> Output a) -> IO ([Output a], Maybe Handle)
nextOut Nothing _ _ = return ([], Nothing)	-- Handle is closed
nextOut _ EndOfFile _ = return ([], Nothing)	-- Handle is closed
nextOut handle Unready _ = return ([], handle)	-- Handle is not ready
nextOut (Just handle) Ready constructor =	-- Perform a read 
    do
      a <- hGetNonBlocking handle bufSize
      case length a of
        -- A zero length read, unlike a zero length write, always
        -- means EOF.
        0 -> do hClose handle
                return ([], Nothing)
        -- Got some input
        _n -> return ([constructor a], Just handle)
