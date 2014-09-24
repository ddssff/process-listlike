{-# LANGUAGE MultiParamTypeClasses, PackageImports, ScopedTypeVariables #-}
{-# OPTIONS -Wwarn -Wall -fno-warn-name-shadowing -fno-warn-missing-signatures #-}
-- | Function to run a process and return a lazy list of chunks from
--   standard output, standard error, and at the end of the list an
--   object indicating the process result code.  If neither output
--   handle is ready for reading the process sleeps and tries again,
--   with the sleep intervals increasing from 8 microseconds to a
--   maximum of 0.1 seconds.
module System.Process.ListLike.Ready
    ( readCreateProcessWithExitCode
    , readProcessInterleaved
    , Process
    , readProcessChunks
    , lazyCommand
    , lazyProcess
    ) where

import Control.Applicative ((<$>), (<*>), pure)
import Control.Concurrent (threadDelay)
import Control.Exception
import Data.ListLike (ListLike(length, null), ListLikeIO(hGetNonBlocking))
import Data.Monoid (Monoid, mempty, mconcat, (<>))
import qualified GHC.IO.Exception as E
import Prelude hiding (length, null)
import System.Exit (ExitCode)
import System.Process (ProcessHandle, CreateProcess(..), waitForProcess, shell, proc, createProcess, StdStream(CreatePipe))
import System.IO (Handle, hSetBinaryMode, hReady, hClose)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Process.ListLike.Class (Chunk(..))

-- For the ListLikeIOPlus instance
import qualified Data.ByteString.Lazy as L
import Data.Word (Word8)

-- For the test
import System.IO (hPutStrLn, stderr)

test1 = lazyCommand "yes | cat -n | while read i; do echo $i; sleep 1; done" L.empty  >>= mapM_ (hPutStrLn stderr . show)
test2 = lazyCommand "ls -l /tmp" L.empty  >>= mapM_ (hPutStrLn stderr . show)
test3 = lazyCommand "oneko" L.empty  >>= mapM_ (hPutStrLn stderr . show)

class ListLikeIO a c => ListLikeIOPlus a c where
    hPutNonBlocking :: Handle -> a -> IO a
    chunks :: a -> [a]

instance ListLikeIOPlus L.ByteString Word8 where
    hPutNonBlocking = L.hPutNonBlocking
    -- Yes, This is ugly.  The question is, do we need to feed the
    -- input to the process in chunks or can we fork an input thread
    -- to do the feeding?  Probably...
    chunks = map (L.fromChunks . (: [])) . L.toChunks

-- | This is the type returned by 'System.Process.createProcess' et. al.
type Process = (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)

bufSize = 65536		-- maximum chunk size
uSecs = 8		-- minimum wait time, doubles each time nothing is ready
maxUSecs = 100000	-- maximum wait time (microseconds)

-- | Create a process with 'runInteractiveCommand' and run it with 'readProcessChunks'.
lazyCommand :: ListLikeIOPlus a c => String -> a -> IO [Chunk a]
lazyCommand cmd input =
    createProcess ((shell cmd) {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}) >>= readProcessChunks input

-- | Create a process with 'runInteractiveProcess' and run it with 'readProcessChunks'.
lazyProcess :: ListLikeIOPlus a c =>
               FilePath
            -> [String]
            -> Maybe FilePath
            -> Maybe [(String, String)]
            -> a
            -> IO [Chunk a]
lazyProcess exec args cwd env input =
    createProcess ((proc exec args) {cwd = cwd, env = env, std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}) >>= readProcessChunks input

readCreateProcessWithExitCode
    :: forall a c.
       ListLikeIOPlus a c =>
       CreateProcess   -- ^ process to run
    -> a               -- ^ standard input
    -> IO (ExitCode, a, a) -- ^ exitcode, stdout, stderr, exception
readCreateProcessWithExitCode p input =
    readProcessInterleaved (\ _ -> mempty)
                           (\ c -> (c, mempty, mempty))
                           (\ x -> (mempty, x, mempty))
                           (\ x -> (mempty, mempty, x))
                           (\ _ -> (mempty, mempty, mempty))
                           p input

readProcessInterleaved :: forall a b c. (ListLikeIOPlus a c, Monoid b) =>
                          (ProcessHandle -> b)
                       -> (ExitCode -> b)
                       -> (a -> b)
                       -> (a -> b)
                       -> (Either AsyncException IOError -> b)
                       -> CreateProcess -> a -> IO b
readProcessInterleaved pidf codef outf errf intf p input =
    createProcess (p {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}) >>=
    readProcessChunks input >>=
    return . mconcat . map doChunk
    where
      doChunk (ProcessHandle x) = pidf x
      doChunk (Stdout x) = outf x
      doChunk (Stderr x) = errf x
      doChunk (Exception x) = intf x
      doChunk (Result x) = codef x

-- | Take a tuple like that returned by 'runInteractiveProcess',
-- create a process, send the list of inputs to its stdin and return
-- the lazy list of 'Output' objects.
readProcessChunks :: ListLikeIOPlus a c => a -> Process -> IO [Chunk a]
readProcessChunks input (mb_inh, mb_outh, mb_errh, pid) =
    (<>) <$> pure [ProcessHandle pid]
         <*> do maybe (return ()) (`hSetBinaryMode` True) mb_inh
                maybe (return ()) (`hSetBinaryMode` True) mb_outh
                maybe (return ()) (`hSetBinaryMode` True) mb_errh
                elements (chunks input, mb_inh, mb_outh, mb_errh, [])
    where
      elements :: ListLikeIOPlus a c => ([a], Maybe Handle, Maybe Handle, Maybe Handle, [Chunk a]) -> IO [Chunk a]
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
ready :: ListLikeIOPlus a c =>
         Int -> ([a], Maybe Handle, Maybe Handle, Maybe Handle, [Chunk a])
      -> IO ([a], Maybe Handle, Maybe Handle, Maybe Handle, [Chunk a])
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
            | null input -> ready waitUSecs (etc, inh, outh, errh, elems)
            -- Send some input to the process
            | True ->
                do input' <- hPutNonBlocking handle input
                   case null input' of
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
nextOut :: ListLikeIO a c => (Maybe Handle) -> Readyness -> (a -> Chunk a) -> IO ([Chunk a], Maybe Handle)
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
