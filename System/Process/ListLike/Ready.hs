{-# LANGUAGE PackageImports, ScopedTypeVariables #-}
{-# OPTIONS -Wwarn -Wall -fno-warn-name-shadowing -fno-warn-missing-signatures #-}
-- | Function to run a process and return a lazy list of chunks from
--   standard output, standard error, and at the end of the list an
--   object indicating the process result code.  If neither output
--   handle is ready for reading the process sleeps and tries again,
--   with the sleep intervals increasing from 8 microseconds to a
--   maximum of 0.1 seconds.
module System.Process.ListLike.Ready
    ( Process
    , lazyRun
    , lazyCommand
    , lazyProcess
    ) where

import Control.Concurrent (threadDelay)
import Control.Exception
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified GHC.IO.Exception as E
import System.Process (ProcessHandle, CreateProcess(..), waitForProcess, shell, proc, createProcess, StdStream(CreatePipe))
import System.IO (Handle, hSetBinaryMode, hReady, hClose)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Process.ListLike.Class (Chunk(..))

-- | This is the type returned by 'System.Process.createProcess' et. al.
type Process = (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)

bufSize = 65536		-- maximum chunk size
uSecs = 8		-- minimum wait time, doubles each time nothing is ready
maxUSecs = 100000	-- maximum wait time (microseconds)

-- | Create a process with 'runInteractiveCommand' and run it with 'lazyRun'.
lazyCommand :: String -> L.ByteString -> IO [Chunk L.ByteString]
lazyCommand cmd input =
    createProcess ((shell cmd) {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}) >>= lazyRun input

-- | Create a process with 'runInteractiveProcess' and run it with 'lazyRun'.
lazyProcess :: FilePath
            -> [String]
            -> Maybe FilePath
            -> Maybe [(String, String)]
            -> L.ByteString
            -> IO [Chunk L.ByteString]
lazyProcess exec args cwd env input =
    createProcess ((proc exec args) {cwd = cwd, env = env, std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}) >>= lazyRun input

-- | Take a tuple like that returned by 'runInteractiveProcess',
-- create a process, send the list of inputs to its stdin and return
-- the lazy list of 'Output' objects.
lazyRun :: L.ByteString -> Process -> IO [Chunk L.ByteString]
lazyRun input (Just inh, Just outh, Just errh, pid) =
    hSetBinaryMode inh True >>
    hSetBinaryMode outh True >>
    hSetBinaryMode errh True >>
    elements (map (L.fromChunks . (: [])) (L.toChunks input), Just inh, Just outh, Just errh, [])
    where
      elements :: ([L.ByteString], Maybe Handle, Maybe Handle, Maybe Handle, [Chunk L.ByteString]) -> IO [Chunk L.ByteString]
      -- EOF on both output descriptors, get exit code.  It can be
      -- argued that the list will always contain exactly one exit
      -- code if traversed to its end, because the only case of
      -- elements that does not recurse is the one that adds a Result,
      -- and there is nowhere else where a Result is added.  However,
      -- the process doing the traversing may die before that end is
      -- reached.
      elements (_, _, Nothing, Nothing, elems) =
          do result <- waitForProcess pid
             -- Note that there is no need to insert the result code
             -- at the end of the list.
             return $ Result result : elems
      -- The available output has been processed, send input and read
      -- from the ready handles
      elements tl@(_, _, _, _, []) = ready uSecs tl >>= elements
      -- Add some output to the result value
      elements (input, inh, outh, errh, elems) =
          do
            etc <- unsafeInterleaveIO (elements (input, inh, outh, errh, []))
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
ready :: Int -> ([L.ByteString], Maybe Handle, Maybe Handle, Maybe Handle, [Chunk L.ByteString])
      -> IO ([L.ByteString], Maybe Handle, Maybe Handle, Maybe Handle, [Chunk L.ByteString])
ready waitUSecs (input, inh, outh, errh, elems) =
    do
      outReady <- maybe (return Unready) hReady' outh
      errReady <- maybe (return Unready) hReady' errh
      case (input, inh, outReady, errReady) of
        -- Input exhausted, close the input handle.
        ([], Just handle, Unready, Unready) ->
            do hClose handle
               ready  waitUSecs ([], Nothing, outh, errh, elems)
        -- Input handle closed and there are no ready output handles,
        -- wait a bit
        ([], Nothing, Unready, Unready) ->
            do threadDelay waitUSecs
               --ePut0 ("Slept " ++ show uSecs ++ " microseconds\n")
               ready (min maxUSecs (2 * waitUSecs)) (input, inh, outh, errh, elems)
        -- Input is available and there are no ready output handles
        (input : etc, Just handle, Unready, Unready)
            -- Discard a zero byte input
            | input == L.empty -> ready waitUSecs (etc, inh, outh, errh, elems)
            -- Send some input to the process
            | True ->
                do input' <- L.hPutNonBlocking handle input
                   case L.null input' of
                     -- Input buffer is full too, sleep.
                     True -> do threadDelay uSecs
                                ready (min maxUSecs (2 * waitUSecs)) (input : etc, inh, outh, errh, elems)
                     -- We wrote some input, discard it and continue
                     False -> return (input' : etc, Just handle, outh, errh, elems)
        -- One or both output handles are ready, try to read from them
        _ ->
            do (out1, errh') <- nextOut errh errReady Stderr
               (out2, outh') <- nextOut outh outReady Stdout
               return (input, inh, outh', errh', elems ++ out1 ++ out2)

-- | Return the next output element and the updated handle
-- from a handle which is assumed ready.
nextOut :: (Maybe Handle) -> Readyness -> (L.ByteString -> Chunk L.ByteString) -> IO ([Chunk L.ByteString], Maybe Handle)
nextOut Nothing _ _ = return ([], Nothing)	-- Handle is closed
nextOut _ EndOfFile _ = return ([], Nothing)	-- Handle is closed
nextOut handle Unready _ = return ([], handle)	-- Handle is not ready
nextOut (Just handle) Ready constructor =	-- Perform a read 
    do
      a <- L.hGetNonBlocking handle bufSize
      case L.length a of
        -- A zero length read, unlike a zero length write, always
        -- means EOF.
        0 -> do hClose handle
                return ([], Nothing)
        -- Got some input
        _n -> return ([constructor a], Just handle)
