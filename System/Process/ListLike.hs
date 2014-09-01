-- | Generalized versions of the functions
-- 'System.Process.readProcess', and
-- 'System.Process.readProcessWithExitCode'.
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies #-}
module System.Process.ListLike (
  ListLikePlus(..),
  readProcessInterleaved,
  readInterleaved,
  readCreateProcessWithExitCode,
  readCreateProcess,
  readProcessWithExitCode,
  readProcess,
  Output(..),
  readProcessChunks,
  showCmdSpecForUser
  ) where

import Control.Concurrent
import Control.DeepSeq (NFData)
import Control.Exception as E (SomeException, onException, evaluate, catch, try, throwIO, mask, throw)
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Int (Int64)
import Data.List as List (map)
import Data.ListLike (ListLike(..), ListLikeIO(..))
import Data.ListLike.Text.Text ()
import Data.ListLike.Text.TextLazy ()
import Data.Monoid (Monoid(mempty, mappend), (<>))
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Word (Word8)
import GHC.IO.Exception (IOErrorType(OtherError, ResourceVanished), IOException(ioe_type))
import Prelude hiding (null, length, rem)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.IO hiding (hPutStr, hGetContents)
import qualified System.IO.Error as IO
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Process (ProcessHandle, CreateProcess(..), StdStream(CreatePipe, Inherit), proc,
                       CmdSpec(RawCommand, ShellCommand), showCommandForUser,
                       createProcess, waitForProcess, terminateProcess)

-- | Class of types which can be used as the input and outputs of
-- these process functions.
class (Integral (LengthType a), ListLikeIO a c) => ListLikePlus a c where
  type LengthType a
  setModes :: a -> (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO ()
  -- ^ Perform initialization on the handles returned by createProcess
  -- based on this ListLikePlus instance - typically setting binary
  -- mode on all the file descriptors if the element type is Word8.
  -- If this is not done, reading something other than text (such as a
  -- .jpg or .pdf file) will usually fail with a decoding error.
  lazy :: a -> Bool
  -- ^ Is this a lazy type?  If so a force is performed in the thread
  -- reading process output.
  length' :: a -> LengthType a
  -- ^ Return the length of the input (this will force it.)
  toChunks :: a -> [a]
  -- ^ Convert the value to a list of chunks.  This is usually
  -- a call to a lazy type's toChunks function, for strict types
  -- it just returns a singleton list.

-- | Read the output of a process and use the argument functions to
-- convert it into a Monoid, preserving the order of appearance of the
-- different chunks of output from standard output and standard error.
readProcessInterleaved :: (ListLikePlus a c, Monoid b) =>
                          (ProcessHandle -> b) -> (ExitCode -> b) -> (a -> b) -> (a -> b)
                       -> CreateProcess -> a -> IO b
readProcessInterleaved pidf codef outf errf p input = mask $ \ restore -> do
  hs@(Just inh, Just outh, Just errh, pid) <-
      createProcess (p {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe })

  setModes input hs

  flip onException
    (do hClose inh; hClose outh; hClose errh;
        terminateProcess pid; waitForProcess pid) $ restore $ do
    waitOut <- forkWait $ readInterleaved (pidf pid) [(outf, outh), (errf, errh)] $ waitForProcess pid >>= return . codef
    writeInput inh input
    waitOut

-- | Simultaneously read the output from several file handles, using
-- the associated functions to add them to a Monoid b in the order
-- they appear.  This closes each handle on EOF, because AFAIK it is
-- the only useful thing to do with a file handle that has reached
-- EOF.
readInterleaved :: forall a b c. (ListLikePlus a c, Monoid b) => b -> [(a -> b, Handle)] -> IO b -> IO b
readInterleaved start pairs finish = newEmptyMVar >>= readInterleaved' start pairs finish

readInterleaved' :: forall a b c. (ListLikePlus a c, Monoid b) =>
                    b -> [(a -> b, Handle)] -> IO b -> MVar (Either Handle b) -> IO b
readInterleaved' start pairs finish res = do
  mapM_ (forkIO . uncurry readHandle) pairs
  r <- takeChunks (length pairs)
  return $ start <> r
    where
      readHandle f h = do
        cs <- hGetContents h
        -- If the type returned as stdout and stderr is lazy we need
        -- to force it here in the producer thread - I'm not exactly
        -- sure why.  And why is String lazy?
        when (lazy (undefined :: a)) (void $ force' cs)
        mapM_ (\ c -> putMVar res (Right (f c))) (toChunks cs)
        hClose h
        putMVar res (Left h)
      takeChunks :: Int -> IO b
      takeChunks 0 = finish
      takeChunks openCount = takeMVar res >>= takeChunk openCount
      takeChunk :: Int -> Either Handle b -> IO b
      takeChunk openCount (Left h) = hClose h >> takeChunks (openCount - 1)
      takeChunk openCount (Right x) =
          do xs <- unsafeInterleaveIO $ takeChunks openCount
             return (x <> xs)

-- | An implementation of 'System.Process.readProcessWithExitCode'
-- with a two generalizations: (1) The input and outputs can be any
-- instance of 'ListLikePlus', and (2) The CreateProcess is passes an
-- argument, so you can use either 'System.Process.proc' or
-- 'System.Process.rawSystem' and you can modify its fields such as
-- 'System.Process.cwd' before the process starts
readCreateProcessWithExitCode
    :: forall a c.
       ListLikePlus a c =>
       CreateProcess   -- ^ process to run
    -> a               -- ^ standard input
    -> IO (ExitCode, a, a) -- ^ exitcode, stdout, stderr, exception
readCreateProcessWithExitCode p input =
    readProcessInterleaved (\ _ -> mempty)
                           (\ c -> (c, mempty, mempty))
                           (\ x -> (mempty, x, mempty))
                           (\ x -> (mempty, mempty, x))
                           p input

-- | A version of 'System.Process.readProcessWithExitCode' that uses
-- any instance of 'ListLikePlus' instead of 'String', implemented
-- using 'readCreateProcessWithExitCode'.
readProcessWithExitCode
    :: ListLikePlus a c =>
       FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> a               -- ^ standard input
    -> IO (ExitCode, a, a) -- ^ exitcode, stdout, stderr
readProcessWithExitCode cmd args input = readCreateProcessWithExitCode (proc cmd args) input

-- | Implementation of 'System.Process.readProcess' that uses any
-- instance of 'ListLikePlus' instead of 'String', implemented using
-- 'readCreateProcess'.
readProcess
    :: ListLikePlus a c =>
       FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> a               -- ^ standard input
    -> IO a            -- ^ stdout
readProcess cmd args = readCreateProcess (proc cmd args)

-- | An implementation of 'System.Process.readProcess' with a two
-- generalizations: (1) The input and outputs can be any instance of
-- 'ListLikePlus', and (2) The CreateProcess is passes an argument, so
-- you can use either 'System.Process.proc' or
-- 'System.Process.rawSystem' and you can modify its fields such as
-- 'System.Process.cwd' before the process starts
--
-- This can't be implemented by calling readProcessInterleaved because
-- the std_err field needs to be set to Inherit, which means that
-- 'createProcess' returns no stderr handle.  Instead, we have a
-- nearly identical copy of the 'readProcessInterleaved' code which
-- only passes one pair 'readInterleaved'.  REMEMBER to keep these two
-- in sync if there are future changes!
readCreateProcess
    :: ListLikePlus a c =>
       CreateProcess   -- ^ process to run
    -> a               -- ^ standard input
    -> IO a            -- ^ stdout
readCreateProcess p input = mask $ \restore -> do
  hs@(Just inh, Just outh, _, pid) <-
      createProcess (p {std_in = CreatePipe, std_out = CreatePipe, std_err = Inherit })

  setModes input hs

  flip onException
    (do hClose inh; hClose outh;
        terminateProcess pid; waitForProcess pid) $ restore $ do
    waitOut <- forkWait $ readInterleaved mempty [(id, outh)] $ waitForProcess pid >>= codef
    writeInput inh input
    waitOut

    where
      -- Throw an IO error on ExitFailure
      codef (ExitFailure r) = throw (mkError "readCreateProcess: " (cmdspec p) r)
      codef ExitSuccess = return mempty

-- | Write and flush process input, closing the handle when done.
-- Catch and ignore Resource Vanished exceptions, they just mean the
-- process exited before all of its output was read.
writeInput :: ListLikePlus a c => Handle -> a -> IO ()
writeInput inh input = do
  (do unless (null input) (hPutStr inh input >> hFlush inh)
      hClose inh) `E.catch` resourceVanished (\ _ -> return ())

-- | Wrapper for a process that provides a handler for the
-- ResourceVanished exception.  This is frequently an exception we
-- wish to ignore, because many processes will deliberately exit
-- before they have read all of their input.
resourceVanished :: (IOError -> IO a) -> IOError -> IO a
resourceVanished epipe e = if ioe_type e == ResourceVanished then epipe e else ioError e

-- | Fork a thread to read from a handle.
forkWait :: IO a -> IO (IO a)
forkWait a = do
  res <- newEmptyMVar
  _ <- mask $ \restore -> forkIO $ try (restore a) >>= putMVar res
  return (takeMVar res >>= either (\ex -> throwIO (ex :: SomeException)) return)

-- | Create an exception for a process that exited abnormally.
mkError :: String -> CmdSpec -> Int -> IOError
mkError prefix (RawCommand cmd args) r =
    IO.mkIOError OtherError (prefix ++ showCommandForUser cmd args ++ " (exit " ++ show r ++ ")")
                 Nothing Nothing
mkError prefix (ShellCommand cmd) r =
    IO.mkIOError OtherError (prefix ++ cmd ++ " (exit " ++ show r ++ ")")
                 Nothing Nothing

force' :: forall a c. ListLikePlus a c => a -> IO (LengthType a)
force' x = evaluate $ length' $ x

instance ListLikePlus String Char where
  type LengthType String = Int
  setModes _ _  = return ()
  lazy _ = True  -- Not quite sure why this is True.
  length' = length
  toChunks = (: [])

instance ListLikePlus B.ByteString Word8 where
  type LengthType B.ByteString = Int
  setModes _ (inh, outh, errh, _) = f inh >> f outh >> f errh where f mh = maybe (return ()) (\ h -> hSetBinaryMode h True) mh
  lazy _ = False
  length' = B.length
  toChunks = (: [])

instance ListLikePlus L.ByteString Word8 where
  type LengthType L.ByteString = Int64
  setModes _ (inh, outh, errh, _) = f inh >> f outh >> f errh where f mh = maybe (return ()) (\ h -> hSetBinaryMode h True) mh
  lazy _ = True
  length' = L.length
  toChunks = List.map (L.fromChunks . (: [])) . L.toChunks

instance ListLikePlus T.Text Char where
  type LengthType T.Text = Int
  setModes _ _  = return ()
  lazy _ = False
  length' = T.length
  toChunks = (: [])

instance ListLikePlus LT.Text Char where
  type LengthType LT.Text = Int64
  setModes _ _  = return ()
  lazy _ = True
  length' = LT.length
  toChunks = List.map (LT.fromChunks . (: [])) . LT.toChunks

instance Monoid ExitCode where
    mempty = ExitFailure 0
    mappend x (ExitFailure 0) = x
    mappend _ x = x

-- | This lets us use DeepSeq's 'Control.DeepSeq.force' on the stream
-- of data returned by 'readProcessChunks'.
instance NFData ExitCode

-- | The output stream of a process returned by 'readProcessChunks'.
data Output a
    = ProcessHandle ProcessHandle -- ^ This will always come first
    | Stdout a
    | Stderr a
    | Exception IOError
    | Result ExitCode
    deriving Show

-- Is this rude?  It will collide with any other bogus Show
-- ProcessHandle instances created elsewhere.
instance Show ProcessHandle where
    show _ = "<processhandle>"

-- | A concrete use of 'readProcessInterleaved' - build a list
-- containing chunks of process output, any exceptions that get thrown
-- (unimplemented), and finally an exit code.
readProcessChunks :: (ListLikePlus a c) => CreateProcess -> a -> IO [Output a]
readProcessChunks p input =
    readProcessInterleaved (\ h -> [ProcessHandle h]) (\ x -> [Result x]) (\ x -> [Stdout x]) (\ x -> [Stderr x]) p input

showCmdSpecForUser :: CmdSpec -> String
showCmdSpecForUser (ShellCommand s) = s
showCmdSpecForUser (RawCommand p args) = showCommandForUser p args
