{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}
module System.Process.Chunks
    ( Chunk(..)
    , readProcessChunks
    -- * Control
    , foldChunk
    , foldChunks
    , putChunk
    -- * Canonical
    , canonicalChunks
    -- * Indent
    , indentChunks
    , putIndented
    -- * Dotify
    , dotifyChunk
    , dotifyChunks
    , putDots
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.DeepSeq (NFData)
import Control.Monad.State (StateT, evalStateT, evalState, get, put)
import Control.Monad.Trans (lift)
import Data.List (foldl')
import Data.ListLike (ListLike(..), ListLikeIO(..))
import Data.Monoid ((<>))
import Prelude hiding (mapM, putStr, null, tail, break, sequence, length, replicate, rem)
import System.Exit (ExitCode)
import System.IO (stderr)
import System.Process (ProcessHandle, CreateProcess)
import System.Process.ListLike (ListLikePlus(LengthType), readProcessInterleaved)

-- | This lets us use DeepSeq's 'Control.DeepSeq.force' on the stream
-- of data returned by 'readProcessChunks'.
instance NFData ExitCode

-- | The output stream of a process returned by 'readProcessChunks'.
data Chunk a
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
readProcessChunks :: (ListLikePlus a c) => CreateProcess -> a -> IO [Chunk a]
readProcessChunks p input =
    readProcessInterleaved (\ h -> [ProcessHandle h]) (\ x -> [Result x]) (\ x -> [Stdout x]) (\ x -> [Stderr x]) p input

foldChunk :: (ProcessHandle -> b) -> (a -> b) -> (a -> b) -> (IOError -> b) -> (ExitCode -> b) -> Chunk a -> b
foldChunk pidf _ _ _ _ (ProcessHandle x) = pidf x
foldChunk _ outf _ _ _ (Stdout x) = outf x
foldChunk _ _ errf _ _ (Stderr x) = errf x
foldChunk _ _ _ exnf _ (Exception x) = exnf x
foldChunk _ _ _ _ exitf (Result x) = exitf x

-- | Build a value from a chunk stream.
foldChunks :: (r -> Chunk a -> r) -> r -> [Chunk a] -> r
foldChunks f r0 xs = foldl' f r0 xs

-- | Write the Stdout chunks to stdout and the Stderr chunks to stderr.
putChunk :: ListLikePlus a c => Chunk a -> IO ()
putChunk (Stdout x) = putStr x
putChunk (Stderr x) = hPutStr stderr x
putChunk _ = return ()

-- | Merge adjacent Stdout or Stderr chunks.
canonicalChunks :: ListLikePlus a c => [Chunk a] -> [Chunk a]
canonicalChunks [] = []
canonicalChunks (Stdout a : Stdout b : more) = canonicalChunks (Stdout (a <> b) : more)
canonicalChunks (Stderr a : Stderr b : more) = canonicalChunks (Stderr (a <> b) : more)
canonicalChunks (a : more) = a : canonicalChunks more

-- | The monad state, are we at the beginning of a line or the middle?
data BOL = BOL | MOL deriving (Eq)

-- | Indent the text of a chunk with the prefixes given for stdout and
-- stderr.  The state monad keeps track of whether we are at the
-- beginning of a line - when we are and more text comes we insert one
-- of the prefixes.
indentChunk :: forall a c m. (Monad m, Functor m, ListLikePlus a c, Eq c) => c -> a -> a -> Chunk a -> StateT BOL m [Chunk a]
indentChunk nl outp errp chunk =
    case chunk of
      Stdout x -> doText Stdout outp x
      Stderr x -> doText Stderr errp x
      _ -> return [chunk]
    where
      doText :: (a -> Chunk a) -> a -> a -> StateT BOL m [Chunk a]
      doText con pre x = do
        let (hd, tl) = break (== nl) x
        (<>) <$> doHead con pre hd <*> doTail con pre tl
      doHead :: (a -> Chunk a) -> a -> a -> StateT BOL m [Chunk a]
      doHead _ _ x | null x = return []
      doHead con pre x = do
        bol <- get
        case bol of
          BOL -> put MOL >> return [con (pre <> x)]
          MOL -> return [con x]
      doTail :: (a -> Chunk a) -> a -> a -> StateT BOL m [Chunk a]
      doTail _ _ x | null x = return []
      doTail con pre x = do
        bol <- get
        put BOL
        tl <- doText con pre (tail x)
        return $ (if bol == BOL then [con pre] else []) <> [con (singleton nl)] <> tl

-- | Pure function to indent the text of a chunk list.
indentChunks :: (ListLikePlus a c, Eq c) => c -> a -> a -> [Chunk a] -> [Chunk a]
indentChunks nl outp errp chunks =
    evalState (Prelude.concat <$> mapM (indentChunk nl outp errp) chunks) BOL

-- | Output the indented text of a chunk list, but return the original
-- unindented list.
putIndented :: (ListLikePlus a c, Eq c) => c -> a -> a -> [Chunk a] -> IO [Chunk a]
putIndented nl outp errp chunks =
    evalStateT (mapM (\ x -> indentChunk nl outp errp x >>= mapM_ (lift . putChunk) >> return x) chunks) BOL

dotifyChunk :: forall a c m. (Monad m, Functor m, ListLikePlus a c) => LengthType a -> c -> Chunk a -> StateT Int m [Chunk a]
dotifyChunk charsPerDot dot chunk =
    case chunk of
      Stdout x -> doChars (length x)
      Stderr x -> doChars (length x)
      _ -> return [chunk]
    where
      doChars count = do
        rem <- get
        let (count', rem') = divMod (rem + count) (fromIntegral charsPerDot)
        put rem'
        if (count' > 0) then return [Stderr (replicate count' dot)] else return []

dotifyChunks :: forall a c. (ListLikePlus a c) => LengthType a -> c -> [Chunk a] -> [Chunk a]
dotifyChunks charsPerDot dot chunks =
    evalState (Prelude.concat <$> mapM (dotifyChunk charsPerDot dot) chunks) 0

-- | Output the dotified text of a chunk list, but return the original
-- unindented list.
putDots :: (ListLikePlus a c) => LengthType a -> c -> [Chunk a] -> IO [Chunk a]
putDots charsPerDot dot chunks =
    evalStateT (mapM (\ x -> dotifyChunk charsPerDot dot x >>= mapM_ (lift . putChunk) >> return x) chunks) 0
