-- | Read the output of several file descriptors concurrently,
-- preserving the order in which the chunks are read.
{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall #-}
module System.Process.Read.Interleaved
    ( Chunked(..)
    , readProcessInterleaved
    , readInterleaved
    ) where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as ByteString.Lazy
import Data.List as List (map)
import Data.Text (Text)
import qualified Data.Text.Lazy as Text.Lazy (Text, toChunks, fromChunks)
import Data.Word (Word8)
import Prelude hiding (null, length, rem)
import System.Process.Read (ListLikePlus(..))

import Control.Concurrent (forkIO, MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Exception as E (onException, catch, mask, try, throwIO, SomeException)
import Control.Monad (unless)
import Data.ListLike (ListLike(..), ListLikeIO(..))
import Data.Monoid (Monoid(mempty), (<>))
import GHC.IO.Exception (IOErrorType(ResourceVanished), IOException(ioe_type))
import Prelude hiding (null, length, rem)
import System.Exit (ExitCode)
import System.IO hiding (hPutStr, hGetContents)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Process (CreateProcess(..), StdStream(CreatePipe),
                       createProcess, waitForProcess, terminateProcess)

-- | Class of types which can also be used by 'System.Process.Read.readProcessChunks'.
class ListLikePlus a c => Chunked a c where
  toChunks :: a -> [a]

instance Chunked ByteString.Lazy.ByteString Word8 where
  toChunks = List.map (ByteString.Lazy.fromChunks . (: [])) . ByteString.Lazy.toChunks

-- I have to assume that this is not prone to utf8 decode errors.
-- When I was converting lazy ByteStrings to Text  I sometimes got
-- such errors when a chunk ended in the middle of a unicode char.
instance Chunked Text.Lazy.Text Char where
  toChunks = List.map (Text.Lazy.fromChunks . (: [])) . Text.Lazy.toChunks

-- Note that the instances that use @toChunks = (: [])@ put everything
-- into a single chunk - lazy reading won't work with these types.
instance Chunked ByteString.ByteString Word8 where
  toChunks = (: [])

instance Chunked String Char where
  toChunks = (: [])

instance Chunked Data.Text.Text Char where
  toChunks = (: [])

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

    waitOut <- forkWait $ do result <- readInterleaved [(outf, outh), (errf, errh)]
                             code <- waitForProcess pid
                             return (result <> codef code)

    -- now write and flush any input
    (do unless (null input) (hPutStr inh input >> hFlush inh)
        hClose inh) `catch` resourceVanished (\ _e -> return ())

    -- wait on the output
    waitOut

-- | Simultaneously read the output from all the handles, using the
-- associated functions to add them to the Monoid b in the order they
-- appear.  Close each handle on EOF (even though they were open when
-- we received them.)
readInterleaved :: forall a b c. (Chunked a c, Monoid b) => [(a -> b, Handle)] -> IO b
readInterleaved pairs = newEmptyMVar >>= readInterleaved' pairs

readInterleaved' :: forall a b c. (Chunked a c, Monoid b) =>
                    [(a -> b, Handle)] -> MVar (Either Handle b) -> IO b
readInterleaved' pairs res = do
  mapM_ (forkIO . uncurry readHandle) pairs
  takeChunks (length pairs)
    where
      readHandle f h = do
        cs <- hGetContents h
        mapM_ (\ c -> putMVar res (Right (f c))) (toChunks cs)
        hClose h
        putMVar res (Left h)
      takeChunks :: Int -> IO b
      takeChunks 0 = return mempty
      takeChunks openCount = takeMVar res >>= takeChunk openCount
      takeChunk :: Int -> Either Handle b -> IO b
      takeChunk openCount (Left h) = hClose h >> takeChunks (openCount - 1)
      takeChunk openCount (Right x) =
          do xs <- unsafeInterleaveIO $ takeChunks openCount
             return (x <> xs)

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
