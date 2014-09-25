-- | Generalized versions of the functions
-- 'System.Process.readProcess', and
-- 'System.Process.readProcessWithExitCode'.
{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies, UndecidableInstances #-}
module System.Process.ListLike.Thread (
  readProcessInterleaved,
  readInterleaved,
  readCreateProcessWithExitCode,
  readCreateProcess,
  readProcessWithExitCode,
  readProcess,
  ) where

import Control.Applicative ((<$>), (<*>), pure)
import Control.Concurrent
import Control.Exception as E (SomeException, onException, catch, try, throwIO, mask, throw, AsyncException(UserInterrupt))
import Control.Monad
import Data.ListLike (ListLike(..), ListLikeIO(..))
import Data.ListLike.Text.Text ()
import Data.ListLike.Text.TextLazy ()
import Data.Monoid (Monoid(mempty, mappend), mconcat, (<>))
import GHC.IO.Exception (IOErrorType(OtherError, ResourceVanished), IOException(ioe_type))
import Prelude hiding (null, length, rem)
import System.Exit (ExitCode(ExitSuccess))
import System.IO hiding (hPutStr, hGetContents)
import qualified System.IO.Error as IO
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Process (ProcessHandle, CreateProcess(..), StdStream(CreatePipe, Inherit), proc,
                       createProcess, waitForProcess, terminateProcess)
import System.Process.ListLike.Class (ListLikePlus(..), ProcessOutput(..))
import System.Process.ListLike.Instances ()

-- | Read the output of a process and use the argument functions to
-- convert it into a Monoid, preserving the order of appearance of the
-- different chunks of output from standard output and standard error.
readProcessInterleaved :: (ListLikePlus a c, ProcessOutput a b) => (ProcessHandle -> IO ()) -> CreateProcess -> a -> IO b
readProcessInterleaved  start p input = mask $ \ restore -> do
  hs@(Just inh, Just outh, Just errh, pid) <-
      createProcess (p {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe })

  start pid
  setModes input hs

  onException
    (restore $
     do waitOut <- forkWait $ (<>) <$> pure (pidf pid)
                                   <*> readInterleaved [(outf, outh), (errf, errh)] (codef <$> waitForProcess pid)
        writeInput inh input
        waitOut)
    (do hClose inh; hClose outh; hClose errh;
        terminateProcess pid; waitForProcess pid)

-- | Simultaneously read the output from several file handles, using
-- the associated functions to add them to a Monoid b in the order
-- they appear.  This closes each handle on EOF, because AFAIK it is
-- the only useful thing to do with a file handle that has reached
-- EOF.
readInterleaved :: forall a b c. (ListLikePlus a c, ProcessOutput a b) =>
                   [(a -> b, Handle)] -> IO b -> IO b
readInterleaved pairs finish = newEmptyMVar >>= readInterleaved' pairs finish

readInterleaved' :: forall a b c. (ListLikePlus a c, ProcessOutput a b) =>
                    [(a -> b, Handle)] -> IO b -> MVar (Either Handle b) -> IO b
readInterleaved' pairs finish res = do
  mapM_ (forkIO . uncurry readHandle) pairs
  takeChunks (length pairs)
    where
      -- Forked thread to read the input and send it to takeChunks via
      -- the MVar.
      readHandle :: (a -> b) -> Handle -> IO ()
      readHandle f h = do
        cs <- readChunks h
        -- If the type returned as stdout and stderr is lazy we need
        -- to force it here in the producer thread - I'm not exactly
        -- sure why.  And why is String lazy?
        -- when (lazy (undefined :: a)) (void cs)
        mapM_ (\ c -> putMVar res (Right (f c))) cs
        hClose h
        putMVar res (Left h)
      takeChunks :: Int -> IO b
      takeChunks 0 = finish
      takeChunks openCount = takeChunk >>= takeMore openCount
      takeMore :: Int -> Either Handle b -> IO b
      takeMore openCount (Left h) = hClose h >> takeChunks (openCount - 1)
      takeMore openCount (Right x) =
          do xs <- unsafeInterleaveIO $ takeChunks openCount
             return (x <> xs)
      takeChunk = takeMVar res `catch` (\ (e :: AsyncException) -> return $ Right $ intf (Left e))

-- | An implementation of 'System.Process.readProcessWithExitCode'
-- with a two generalizations: (1) The input and outputs can be any
-- instance of 'ListLikePlus', and (2) The CreateProcess is passes an
-- argument, so you can use either 'System.Process.proc' or
-- 'System.Process.rawSystem' and you can modify its fields such as
-- 'System.Process.cwd' before the process starts
readCreateProcessWithExitCode :: ListLikePlus a c =>
                                 CreateProcess       -- ^ process to run
                              -> a                   -- ^ standard input
                              -> IO (ExitCode, a, a) -- ^ exitcode, stdout, stderr
readCreateProcessWithExitCode p input = readProcessInterleaved (\ pid -> return ())  p input

-- | A version of 'System.Process.readProcessWithExitCode' that uses
-- any instance of 'ListLikePlus' instead of 'String', implemented
-- using 'readCreateProcessWithExitCode'.
readProcessWithExitCode :: ListLikePlus a c =>
                           FilePath            -- ^ command to run
                        -> [String]            -- ^ any arguments
                        -> a                   -- ^ standard input
                        -> IO (ExitCode, a, a) -- ^ exitcode, stdout, stderr
readProcessWithExitCode cmd args input = readCreateProcessWithExitCode (proc cmd args) input

-- | Implementation of 'System.Process.readProcess' that uses any
-- instance of 'ListLikePlus' instead of 'String', implemented using
-- 'readCreateProcess'.
readProcess :: ListLikePlus a c =>
               FilePath        -- ^ command to run
            -> [String]        -- ^ any arguments
            -> a               -- ^ standard input
            -> IO a            -- ^ stdout
readProcess cmd args = readCreateProcess (proc cmd args)

newtype StdoutWrapper a = StdoutWrapper a

instance Monoid a => Monoid (StdoutWrapper a) where
    mempty = StdoutWrapper mempty
    mappend (StdoutWrapper a) (StdoutWrapper b) = StdoutWrapper (a <> b)

instance (ListLikePlus a c, Monoid a) => ProcessOutput a (StdoutWrapper a) where
    pidf _ = mempty
    codef ExitSuccess = mempty
    codef failure = throw $ IO.mkIOError OtherError ("Process exited with " ++ show failure) Nothing Nothing
    outf x = StdoutWrapper x
    errf _ = mempty
    intf _ = mempty

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
readCreateProcess :: (ListLikePlus a c, ProcessOutput a (StdoutWrapper a)) =>
                     CreateProcess   -- ^ process to run
                  -> a               -- ^ standard input
                  -> IO a            -- ^ stdout
readCreateProcess p input = mask $ \restore -> do
  hs@(Just inh, Just outh, _, pid) <-
      createProcess (p {std_in = CreatePipe, std_out = CreatePipe, std_err = Inherit })

  setModes input hs

  onException
    (restore $
     do waitOut <- forkWait $ (readInterleaved [(StdoutWrapper, outh)] (waitForProcess pid >>= return . codef))
        writeInput inh input
        StdoutWrapper x <- waitOut
        return x)
    (do hClose inh; hClose outh;
        terminateProcess pid; waitForProcess pid)

-- Is this rude?  It will collide with any other bogus Show
-- ProcessHandle instances created elsewhere.
instance Show ProcessHandle where
    show _ = "<processhandle>"

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
