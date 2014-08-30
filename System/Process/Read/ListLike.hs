-- | Versions of the functions in module 'System.Process.Read' specialized for type ByteString.
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies #-}
module System.Process.Read.ListLike (
  ListLikePlus(..),
  Chunked(..),
  readProcessInterleaved,
  readInterleaved,
  readCreateProcessWithExitCode,
  readCreateProcess,
  readProcessWithExitCode,
  readProcess,
  ) where

import Control.Concurrent
import Control.Exception as E (SomeException, onException, evaluate, catch, try, throwIO, mask)
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Int (Int64)
import Data.List as List (map)
import Data.ListLike (ListLike(..), ListLikeIO(..))
import Data.ListLike.Text.Text ()
import Data.ListLike.Text.TextLazy ()
import Data.Monoid (Monoid, (<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Word (Word8)
import GHC.IO.Exception (IOErrorType(OtherError, ResourceVanished), IOException(ioe_type))
import Prelude hiding (null, length, rem)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.IO hiding (hPutStr, hGetContents)
import qualified System.IO.Error as IO
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Process (CreateProcess(..), StdStream(CreatePipe, Inherit), proc,
                       CmdSpec(RawCommand, ShellCommand), showCommandForUser,
                       createProcess, waitForProcess, terminateProcess)

-- | Class of types which can be used as the input and outputs of the process functions.
class (Integral (LengthType a), ListLikeIO a c) => ListLikePlus a c where
  type LengthType a
  binary :: a -> [Handle] -> IO ()
  -- ^ This should call 'hSetBinaryMode' on each handle if a is a
  -- ByteString type, so that it doesn't attempt to decode the text
  -- using the current locale.
  lazy :: a -> Bool
  length' :: a -> LengthType a

-- | Class of types which can also be used by 'System.Process.Read.readProcessChunks'.
class ListLikePlus a c => Chunked a c where
  toChunks :: a -> [a]

-- | A test version of readProcessC
-- Pipes code here: http://hpaste.org/76631
readProcessInterleaved :: (Chunked a c, Monoid b) =>
                          (ExitCode -> b) -> (a -> b) -> (a -> b)
                       -> CreateProcess -> a -> IO b
readProcessInterleaved codef outf errf p input = mask $ \ restore -> do
  (Just inh, Just outh, Just errh, pid) <-
      createProcess (p {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe })

  binary input [inh, outh, errh]

  flip onException
    (do hClose inh; hClose outh; hClose errh;
        terminateProcess pid; waitForProcess pid) $ restore $ do

    waitOut <- forkWait $ readInterleaved [(outf, outh), (errf, errh)] $ waitForProcess pid >>= return . codef

    -- now write and flush any input
    (do unless (null input) (hPutStr inh input >> hFlush inh)
        hClose inh) `catch` resourceVanished (\ _e -> return ())

    -- wait on the output
    waitOut

-- | Simultaneously read the output from all the handles, using the
-- associated functions to add them to the Monoid b in the order they
-- appear.  Close each handle on EOF (even though they were open when
-- we received them.)
readInterleaved :: forall a b c. (Chunked a c, Monoid b) => [(a -> b, Handle)] -> IO b -> IO b
readInterleaved pairs finish = newEmptyMVar >>= readInterleaved' pairs finish

readInterleaved' :: forall a b c. (Chunked a c, Monoid b) =>
                    [(a -> b, Handle)] -> IO b -> MVar (Either Handle b) -> IO b
readInterleaved' pairs finish res = do
  mapM_ (forkIO . uncurry readHandle) pairs
  takeChunks (length pairs)
    where
      readHandle f h = do
        cs <- hGetContents h
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

-- | A polymorphic implementation of
-- 'System.Process.readProcessWithExitCode' with a few
-- generalizations:
--
--    1. The input and outputs can be any instance of 'ListLikePlus'.
--
--    2. Allows you to modify the 'CreateProcess' record before the process starts
--
--    3. Takes a 'CmdSpec', so you can launch either a 'RawCommand' or a 'ShellCommand'.
readCreateProcessWithExitCode
    :: forall a c.
       ListLikePlus a c =>
       CreateProcess   -- ^ process to run
    -> a               -- ^ standard input
    -> IO (ExitCode, a, a) -- ^ exitcode, stdout, stderr, exception
readCreateProcessWithExitCode p input = mask $ \restore -> do
    (Just inh, Just outh, Just errh, pid) <-
        createProcess (p {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe })

    flip onException
      (do hClose inh; hClose outh; hClose errh;
          terminateProcess pid; waitForProcess pid) $ restore $ do
      (out, err) <- (if lazy input then readLazy else readStrict) inh outh errh

      hClose outh
      hClose errh

      -- wait on the process
      ex <- waitForProcess pid

      return (ex, out, err)
    where
      readLazy :: Handle -> Handle -> Handle -> IO (a, a)
      readLazy inh outh errh =
           do out <- hGetContents outh
              waitOut <- forkWait $ void $ force $ out
              err <- hGetContents errh
              waitErr <- forkWait $ void $ force $ err
              -- now write and flush any input
              writeInput inh
              -- wait on the output
              waitOut
              waitErr
              return (out, err)

      readStrict :: Handle -> Handle -> Handle -> IO (a, a)
      readStrict inh outh errh =
           do -- fork off a thread to start consuming stdout
              waitOut <- forkWait $ hGetContents outh
              -- fork off a thread to start consuming stderr
              waitErr <- forkWait $ hGetContents errh
              -- now write and flush any input
              writeInput inh
              -- wait on the output
              out <- waitOut
              err <- waitErr
              return (out, err)

      writeInput :: Handle -> IO ()
      writeInput inh = do
        (do unless (null input) (hPutStr inh input >> hFlush inh)
            hClose inh) `E.catch` resourceVanished (\ _ -> return ())

-- | A polymorphic implementation of
-- 'System.Process.readProcessWithExitCode' in terms of
-- 'readCreateProcessWithExitCode'.
readProcessWithExitCode
    :: ListLikePlus a c =>
       FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> a               -- ^ standard input
    -> IO (ExitCode, a, a) -- ^ exitcode, stdout, stderr
readProcessWithExitCode cmd args input = readCreateProcessWithExitCode (proc cmd args) input

-- | Implementation of 'System.Process.readProcess' in terms of
-- 'readCreateProcess'.
readProcess
    :: ListLikePlus a c =>
       FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> a               -- ^ standard input
    -> IO a            -- ^ stdout
readProcess cmd args = readCreateProcess (proc cmd args)

-- | A polymorphic implementation of 'System.Process.readProcess' with a few generalizations:
--
--    1. The input and outputs can be any instance of 'ListLikePlus'.
--
--    2. Allows you to modify the 'CreateProcess' record before the process starts
--
--    3. Takes a 'CmdSpec', so you can launch either a 'RawCommand' or a 'ShellCommand'.
readCreateProcess
    :: ListLikePlus a c =>
       CreateProcess   -- ^ process to run
    -> a               -- ^ standard input
    -> IO a            -- ^ stdout
readCreateProcess p input = mask $ \restore -> do
    (Just inh, Just outh, _, pid) <-
        createProcess (p {std_in = CreatePipe, std_out = CreatePipe, std_err = Inherit })

    flip onException
      (do hClose inh; hClose outh;
          terminateProcess pid; waitForProcess pid) $ restore $ do
      out <- (if lazy input then readLazy else readStrict) inh outh

      hClose outh

      -- wait on the process
      ex <- waitForProcess pid

      case ex of
        ExitSuccess   -> return out
        ExitFailure r -> ioError (mkError "readCreateProcess: " (cmdspec p) r)
    where
      readLazy inh outh =
          do -- fork off a thread to start consuming stdout
             out <- hGetContents outh
             waitOut <- forkWait $ void $ force $ out
             writeInput inh
             waitOut
             return out

      readStrict inh outh =
          do waitOut <- forkWait $ hGetContents outh
             writeInput inh
             waitOut

      writeInput inh = do
         (do unless (null input) (hPutStr inh input >> hFlush inh)
             hClose inh) `E.catch` resourceVanished (\ _ -> return ())

forkWait :: IO a -> IO (IO a)
forkWait a = do
  res <- newEmptyMVar
  _ <- mask $ \restore -> forkIO $ try (restore a) >>= putMVar res
  return (takeMVar res >>= either (\ex -> throwIO (ex :: SomeException)) return)

-- | Wrapper for a process that provides a handler for the
-- ResourceVanished exception.  This is frequently an exception we
-- wish to ignore, because many processes will deliberately exit
-- before they have read all of their input.
resourceVanished :: (IOError -> IO a) -> IOError -> IO a
resourceVanished epipe e = if ioe_type e == ResourceVanished then epipe e else ioError e

-- | Create an exception for a process that exited abnormally.
mkError :: String -> CmdSpec -> Int -> IOError
mkError prefix (RawCommand cmd args) r =
    IO.mkIOError OtherError (prefix ++ showCommandForUser cmd args ++ " (exit " ++ show r ++ ")")
                 Nothing Nothing
mkError prefix (ShellCommand cmd) r =
    IO.mkIOError OtherError (prefix ++ cmd ++ " (exit " ++ show r ++ ")")
                 Nothing Nothing

force :: forall a c. ListLikePlus a c => a -> IO (LengthType a)
force x = evaluate $ length' $ x

instance ListLikePlus String Char where
  type LengthType String = Int
  binary _ = mapM_ (\ h -> hSetBinaryMode h True) -- Prevent decoding errors when reading handles (because internally this uses lazy bytestrings)
  lazy _ = True
  length' = length

instance ListLikePlus B.ByteString Word8 where
  type LengthType B.ByteString = Int
  binary _ = mapM_ (\ h -> hSetBinaryMode h True) -- Prevent decoding errors when reading handles
  lazy _ = False
  length' = B.length

instance ListLikePlus L.ByteString Word8 where
  type LengthType L.ByteString = Int64
  binary _ = mapM_ (\ h -> hSetBinaryMode h True) -- Prevent decoding errors when reading handles
  lazy _ = True
  length' = L.length

instance ListLikePlus T.Text Char where
  type LengthType T.Text = Int
  binary _ _ = return ()
  lazy _ = False
  length' = T.length

instance ListLikePlus LT.Text Char where
  type LengthType LT.Text = Int64
  binary _ _ = return ()
  lazy _ = True
  length' = LT.length

instance Chunked L.ByteString Word8 where
  toChunks = List.map (L.fromChunks . (: [])) . L.toChunks

-- I have to assume that this is not prone to utf8 decode errors.
-- When I was converting lazy ByteStrings to Text  I sometimes got
-- such errors when a chunk ended in the middle of a unicode char.
instance Chunked LT.Text Char where
  toChunks = List.map (LT.fromChunks . (: [])) . LT.toChunks

-- Note that the instances that use @toChunks = (: [])@ put everything
-- into a single chunk - lazy reading won't work with these types.
instance Chunked B.ByteString Word8 where
  toChunks = (: [])

instance Chunked String Char where
  toChunks = (: [])

instance Chunked Data.Text.Text Char where
  toChunks = (: [])
