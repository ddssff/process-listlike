{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}
module System.Process.Chunks
    ( Chunk(..)
    , readProcessChunks
    , pipeProcessChunks
    -- * Control
    , foldChunk
    , foldChunks
    , putChunk
    -- * Chunk list operators
    , canonicalChunks
    , collectProcessTriple
    , collectProcessOutput'
    , collectProcessOutput
    , collectOutputAndError'
    , collectOutputAndError
    , withProcessResult
    , withProcessException
    , throwProcessResult
    -- * Indent
    , indentChunks
    , putIndented
    , mapIndented
    -- * Dotify
    , dotifyChunk
    , dotifyChunks
    , putDots
    , putDotsLn
    -- * Display command and arguments
    , showCommandChunks
    , putIndentedShowCommand
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.DeepSeq (NFData)
import Control.Monad.State (StateT, evalStateT, evalState, get, put)
import Control.Monad.Trans (lift)
import Data.List (foldl')
import Data.ListLike (ListLike(..), ListLikeIO(..))
import Data.Monoid ((<>), mempty, mconcat)
import Data.String (IsString(fromString))
import GHC.IO.Exception (IOErrorType(OtherError))
import Prelude hiding (mapM, putStr, null, tail, break, sequence, length, replicate, rem)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.IO (stderr)
import System.IO.Error (mkIOError)
import System.Process (ProcessHandle, CreateProcess(cmdspec))
import System.Process.ListLike (ListLikePlus, readProcessInterleaved, showCmdSpecForUser)

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

-- | Merge adjacent and eliminate empty Stdout or Stderr chunks.
canonicalChunks :: ListLikePlus a c => [Chunk a] -> [Chunk a]
canonicalChunks [] = []
canonicalChunks (Stdout a : Stdout b : more) = canonicalChunks (Stdout (a <> b) : more)
canonicalChunks (Stderr a : Stderr b : more) = canonicalChunks (Stderr (a <> b) : more)
canonicalChunks (Stdout a : more) | null a = canonicalChunks more
canonicalChunks (Stderr a : more) | null a = canonicalChunks more
canonicalChunks (a : more) = a : canonicalChunks more

-- | Turn the output of 'readProcessChunks' into the output of
-- 'readProcessWithExitCode'.
collectProcessTriple :: ListLike a c => [Chunk a] -> (ExitCode, a, a)
collectProcessTriple chunks =
    mconcat $ Prelude.map
                (foldChunk
                   (\ _ -> mempty)
                   (\ o -> (mempty, o, mempty))
                   (\ e -> (mempty, mempty, e))
                   (\ _ -> mempty)
                   (\ code -> (code, mempty, mempty))) chunks

-- | Turn the output of 'readProcessChunks' into the output of
-- 'readProcess'.  Remember that this does not throw an exception on
-- exit failure like readProcess, because it is not in the IO monad.
-- Use 'pipeProcessChunks' to do that.
collectProcessOutput :: ListLike a c => [Chunk a] -> a
collectProcessOutput chunks =
    mconcat $ Prelude.map
                (foldChunk
                   (\ _ -> mempty)
                   (\ stdout -> stdout)
                   (\ _ -> mempty)
                   (\ _ -> mempty)
                   (\ _ -> mempty)) chunks

collectOutputAndError :: ListLike a c => [Chunk a] -> a
collectOutputAndError chunks =
    mconcat $ Prelude.map
                (foldChunk
                   (\ _ -> mempty)
                   (\ stdout -> stdout)
                   (\ stderr -> stderr)
                   (\ _ -> mempty)
                   (\ _ -> mempty)) chunks

-- | withProcessResult f input applies f to the result code of input
-- stream, replacing the Result chunk with the return value of f (or,
-- as it may happen, throwing an exception.)  See 'pipeProcessChunks'
-- for an example usage.
withProcessResult :: (ListLike a c, Monad m) => (ExitCode -> m (Chunk a)) -> [Chunk a] -> m [Chunk a]
withProcessResult f input =
    mapM (foldChunk
            (return . ProcessHandle)
            (return . Stdout)
            (return . Stderr)
            (return . Exception)
            f) input

-- | withProcessException f input applies f to the exceptions it finds
-- in the input stream, replacing the Exceptoin chunk with the return
-- value of f (or, as it may happen, throwing an exception.)
withProcessException :: (ListLike a c, Monad m) => (IOError -> m (Chunk a)) -> [Chunk a] -> m [Chunk a]
withProcessException f input =
    mapM (foldChunk
            (return . ProcessHandle)
            (return . Stdout)
            (return . Stderr)
            f
            (return . Result)) input

-- | Turn the standard output of one process into the input of
-- another, throwing an IO exception if the input process result code
-- is ExitFailure.
pipeProcessChunks :: (ListLikePlus a c) => CreateProcess -> [Chunk a] -> IO [Chunk a]
pipeProcessChunks p input = collectProcessOutput' p input >>= readProcessChunks p

-- | A version of collectProcessOutput which throws an exception of
-- the exit code is ExitFailure.
collectProcessOutput' :: ListLike a c => CreateProcess -> [Chunk a] -> IO a
collectProcessOutput' p chunks =
    withProcessResult (throwProcessResult "pipeProcessChunks" (showCmdSpecForUser (cmdspec p))) chunks >>= return . collectProcessOutput

collectOutputAndError' :: ListLike a c => CreateProcess -> [Chunk a] -> IO a
collectOutputAndError' p chunks =
    withProcessResult (throwProcessResult "pipeProcessChunks" (showCmdSpecForUser (cmdspec p))) chunks >>= return . collectOutputAndError

-- | Based on the 'ExitCode', either return a Result Chunk or throw an
-- IO error similar to what 'System.Process.readProcess' would have
-- thrown.
throwProcessResult :: String -> String -> ExitCode -> IO (Chunk a)
throwProcessResult _ _ ExitSuccess = return $ Result ExitSuccess
throwProcessResult fun cmd (ExitFailure code) = processFailedException fun cmd [] code

-- | Copied from "System.Process", the exception thrown when the
-- process started by 'System.Process.readProcess' gets an
-- 'ExitFailure'.
processFailedException :: String -> String -> [String] -> Int -> IO a
processFailedException fun cmd args exit_code =
      ioError (mkIOError OtherError (fun ++ ": " ++ cmd ++
                                     Prelude.concatMap ((' ':) . show) args ++
                                     " (exit " ++ show exit_code ++ ")")
                                 Nothing Nothing)

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
indentChunks :: forall a c. (ListLikePlus a c, Eq c, IsString a) => String -> String -> [Chunk a] -> [Chunk a]
indentChunks outp errp chunks =
    evalState (Prelude.concat <$> mapM (indentChunk nl (fromString outp) (fromString errp)) chunks) BOL
    where
      nl :: c
      nl = Data.ListLike.head (fromString "\n" :: a)

showCommandChunks :: (IsString a, ListLikePlus a c, Eq c) => CreateProcess -> [Chunk a] -> [Chunk a]
showCommandChunks p chunks =
    [Stderr (fromString (" -> " ++ showCmdSpecForUser (cmdspec p) ++ "\n"))] ++
    Prelude.concatMap
      (foldChunk ((: []) . ProcessHandle)
                 ((: []) . Stdout)
                 ((: []) . Stderr)
                 ((: []) . Exception)
                 (\ code -> [Stderr (fromString (" <- " ++ show code ++ " <- " ++ showCmdSpecForUser (cmdspec p) ++ "\n")), Result code])) chunks

putIndentedShowCommand :: (ListLikePlus a c, Eq c, IsString a) => CreateProcess -> String -> String -> [Chunk a] -> IO [Chunk a]
putIndentedShowCommand p outp errp chunks = do
  mapM_ putChunk (showCommandChunks p (indentChunks outp errp chunks))
  return chunks

-- | Output the indented text of a chunk list, but return the original
-- unindented list.  Returns the original chunks.
putIndented :: (ListLikePlus a c, Eq c, IsString a) => String -> String -> [Chunk a] -> IO [Chunk a]
putIndented = mapIndented putChunk

-- | Map chunkfn over the indented chunk stream, returns the original chunks.
mapIndented :: forall a c m. (ListLikePlus a c, Eq c, IsString a, Monad m, Functor m) => (Chunk a -> m ()) -> String -> String -> [Chunk a] -> m [Chunk a]
mapIndented chunkfn outp errp chunks =
    evalStateT (mapM (\ x -> indentChunk nl (fromString outp) (fromString errp) x >>= mapM_ (lift . chunkfn) >> return x) chunks) BOL
    where
      nl :: c
      nl = Data.ListLike.head (fromString "\n" :: a)

-- | dotifyChunk charsPerDot dot chunk - Replaces every charsPerDot
-- characters in the Stdout and Stderr chunks with one dot.  Runs in
-- the state monad to keep track of how many characters had been seen
-- when the previous chunk finished.  chunks.
dotifyChunk :: forall a c m. (Monad m, Functor m, ListLikePlus a c) => Int -> c -> Chunk a -> StateT Int m [Chunk a]
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

dotifyChunks :: forall a c. (ListLikePlus a c) => Int -> c -> [Chunk a] -> [Chunk a]
dotifyChunks charsPerDot dot chunks =
    evalState (Prelude.concat <$> mapM (dotifyChunk charsPerDot dot) chunks) 0

-- | Output the dotified text of a chunk list, but return the original
-- unindented list.
putDots :: (ListLikePlus a c) => Int -> c -> [Chunk a] -> IO [Chunk a]
putDots charsPerDot dot chunks =
    evalStateT (mapM (\ x -> dotifyChunk charsPerDot dot x >>= mapM_ (lift . putChunk) >> return x) chunks) 0

putDotsLn :: (ListLikePlus a c) => Int -> c -> [Chunk a] -> IO [Chunk a]
putDotsLn cpd dot chunks = putDots cpd dot chunks >>= \ r -> hPutStr stderr "\n" >> return r
