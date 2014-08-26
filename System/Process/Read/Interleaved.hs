-- | Read the output of several file descriptors concurrently,
-- preserving the order in which the chunks are read.
{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall #-}
module System.Process.Read.Interleaved
    ( NonBlocking(..)
    , readInterleaved
    ) where

import Control.Concurrent (forkIO, MVar, newEmptyMVar, putMVar, takeMVar)
import Data.ListLike (ListLikeIO(hGetContents))
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.ListLike (ListLike(length))
import Data.Monoid (Monoid, mempty, (<>))
import Data.Word (Word8)
import Prelude hiding (null, length, rem)
import System.IO hiding (hPutStr, hGetContents)
import System.Process.Read (ListLikePlus(..))
import System.IO.Unsafe (unsafeInterleaveIO)

-- | Class of ListLikePlus types which can be used by
-- 'System.Process.Read.readInterleaved'.  This once had a hGetSome
-- method, but the new implementation using MVar makes that
-- unnecessary.
class ListLikePlus a c => NonBlocking a c where
  toChunks :: a -> [a]

{- Its not safe to read a bytestring chunk and then convert it to a
   string, the chunk might end in the middle of an encoded Char.
instance NonBlocking String Char where
  toChunks = error "toChunks" -}

instance NonBlocking B.ByteString Word8 where
  toChunks = (: [])

instance NonBlocking L.ByteString Word8 where
  toChunks = Prelude.map (\ x -> L.fromChunks [x]) . L.toChunks

-- | Read input from a list of handles, using the function supplied
-- with each handle to convert the input into some Monoid.  This is a
-- great example of simple MVar usage, so I'm going to discuss it and
-- refer back to this text when I have concurrency stuff to write.
--
-- The parent process creates the empty mvar named 'res'.  We then
-- fork off threads for each of the handles we want to interleave
-- output from - typically stdout and stderr.  Each of those threads
-- reads the all of the output, converts it into chunks using the
-- 'toChunks' method.  For each chunk, it applies 'f' to convert it to
-- the Monoid type and does a 'putMVar' on the result.
--
-- Meanwhile, the parent process is repeatedly doing 'takeMVar'.  This
-- blocks until one of the threads does a 'putMVar', then it removes
-- the output chunk.  It recursively gets the remaining output and
-- appends the two.
--
-- The object type in the MVar is a Maybe - when this value is a Just
-- we are receiving a chunk, when it is Nothing it means we have
-- reached eof for one of the handles.  The parent process counts the
-- number of Nothings it has received, and when it reaches the number
-- of handles it is interleaving it is done.
readInterleaved :: forall a b c. (NonBlocking a c, Monoid b) => [(a -> b, Handle)] -> IO b
readInterleaved pairs = do
  res <- newEmptyMVar
  mapM_ (\ (f, h) -> forkIO $ sendChunks res f h) pairs
  takeChunks (length pairs) res
    where
      sendChunks :: MVar (Maybe b) -> (a -> b) -> Handle -> IO ()
      sendChunks res f h = hGetContents h >>= mapM_ (sendChunk res f) . toChunks >> hClose h >> putMVar res Nothing
      sendChunk :: MVar (Maybe b) -> (a -> b) -> a -> IO ()
      sendChunk res f c = putMVar res (Just (f c))

      takeChunks :: Int -> MVar (Maybe b) -> IO b
      takeChunks 0 _ = return mempty
      takeChunks openCount res = takeMVar res >>= takeChunk openCount res
      takeChunk :: Int -> MVar (Maybe b) -> Maybe b -> IO b
      takeChunk openCount res Nothing = takeChunks (openCount - 1) res
      takeChunk openCount res (Just x) =
          -- Why unsafeInterleaveIO?
          do xs <- unsafeInterleaveIO $ takeChunks openCount res
             return (x <> xs)
