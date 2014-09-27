-- | Support for using the 'Chunk' list returned by 'readProcessChunks'.
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies, UndecidableInstances #-}
module System.Process.ListLike.Chunks
    ( -- * Basics
      Chunk(..)
    , readProcessChunks
    -- * Folds
    , foldChunk
    , foldChunks
    -- * Pure Chunk List Combinators
    , canonicalChunks'
    , indentChunks
    , dotifyChunks
    , insertCommandStart
    , insertCommandResult
    , insertCommandDisplay
    , eraseStdout
    , eraseStderr
    , eraseOutput
    , mergeToStderr
    , mergeToStdout
    -- * Chunk List Functions
    , collectProcessTriple
    , collectProcessOutput
    , collectOutputAndError
    , collectExitCode
    -- * Monadic Chunk List Combinators
    , withProcessResult
    , withProcessException
    -- * IO
    , putChunk
    , throwProcessResult
    , collectProcessOutput'
    , collectOutputAndError'
    , throwExitCode
    -- * IO Chunk List Combinators
    , pipeProcessChunks
    , putMappedChunks
    , putIndented
    , putDots
    , putDotsLn
    , putIndentedShowCommand
    -- * CreateProcess utility function
    , displayCreateProcess
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.DeepSeq (NFData)
import Control.Exception (SomeException)
import Control.Monad (void)
import Control.Monad.State (StateT, evalStateT, evalState, get, put)
import Control.Monad.Trans (lift)
import Data.List (foldl')
import Data.ListLike (ListLike(..), ListLikeIO(..))
import Data.Monoid (Monoid, (<>), mempty, mconcat)
import Data.String (IsString(fromString))
import GHC.IO.Exception (IOErrorType(OtherError))
import Prelude hiding (mapM, putStr, null, tail, break, sequence, length, replicate, rem)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.IO (stderr)
import System.IO.Error (mkIOError)
import System.Process (ProcessHandle, CreateProcess(cmdspec, cwd))
import System.Process.ListLike.Class (ListLikePlus, ProcessOutput(pidf, outf, errf, intf, codef), showCmdSpecForUser)
import System.Process.ListLike.Thread (readProcessInterleaved)

-- | The output stream of a process returned by 'readProcessChunks'.
data Chunk a
    = ProcessHandle ProcessHandle -- ^ This will always come first
    | Stdout a
    | Stderr a
    | Exception SomeException
    | Result ExitCode
    deriving Show

instance ListLikePlus a c => ProcessOutput a [Chunk a] where
    pidf p = [ProcessHandle p]
    outf x = [Stdout x]
    errf x = [Stderr x]
    intf e = [Exception e]
    codef c = [Result c]

instance ListLikePlus a c => ProcessOutput a (ExitCode, [Chunk a]) where
    pidf _ = mempty
    codef c = (c, mempty)
    outf x = (mempty, [Stdout x])
    errf x = (mempty, [Stderr x])
    intf _ = mempty

-- | This lets us use DeepSeq's 'Control.DeepSeq.force' on the stream
-- of data returned by 'readProcessChunks'.
instance NFData ExitCode

-- | A concrete use of 'readProcessInterleaved' - build a list
-- containing chunks of process output, any exceptions that get thrown
-- (unimplemented), and finally an exit code.
readProcessChunks :: (ListLikePlus a c) => CreateProcess -> a -> IO [Chunk a]
readProcessChunks p input = readProcessInterleaved p input

-- Deprecated - use ProcessOutput instances instead
foldChunk :: (ProcessHandle -> b) -> (a -> b) -> (a -> b) -> (SomeException -> b) -> (ExitCode -> b) -> Chunk a -> b
foldChunk f _ _ _ _ (ProcessHandle x) = f x
foldChunk _ f _ _ _ (Stdout x) = f x
foldChunk _ _ f _ _ (Stderr x) = f x
foldChunk _ _ _ f _ (Exception x) = f x
foldChunk _ _ _ _ f (Result x) = f x

-- | Build a value from a chunk stream.
foldChunks :: (r -> Chunk a -> r) -> r -> [Chunk a] -> r
foldChunks f r0 xs = foldl' f r0 xs

{-
-- | Merge adjacent and eliminate empty Stdout or Stderr chunks.  This
-- may not be a good idea if we are looking to get our output as soon
-- as it becomes available.
canonicalChunks :: ListLikePlus a c => [Chunk a] -> [Chunk a]
canonicalChunks [] = []
canonicalChunks (Stdout a : Stdout b : more) = canonicalChunks (Stdout (a <> b) : more)
canonicalChunks (Stderr a : Stderr b : more) = canonicalChunks (Stderr (a <> b) : more)
canonicalChunks (Stdout a : more) | null a = canonicalChunks more
canonicalChunks (Stderr a : more) | null a = canonicalChunks more
canonicalChunks (a : more) = a : canonicalChunks more
-}

-- | Eliminate empty Stdout or Stderr chunks.
canonicalChunks' :: ListLikePlus a c => [Chunk a] -> [Chunk a]
canonicalChunks' [] = []
canonicalChunks' (Stdout a : more) | null a = canonicalChunks' more
canonicalChunks' (Stderr a : more) | null a = canonicalChunks' more
canonicalChunks' (a : more) = a : canonicalChunks' more

-- | Pure function to indent the text of a chunk list.
indentChunks :: forall a c. (ListLikePlus a c, Eq c, IsString a) => String -> String -> [Chunk a] -> [Chunk a]
indentChunks outp errp chunks =
    evalState (Prelude.concat <$> mapM (indentChunk nl (fromString outp) (fromString errp)) chunks) BOL
    where
      nl :: c
      nl = Data.ListLike.head (fromString "\n" :: a)

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

dotifyChunks :: forall a c. (ListLikePlus a c) => Int -> c -> [Chunk a] -> [Chunk a]
dotifyChunks charsPerDot dot chunks =
    evalState (Prelude.concat <$> mapM (dotifyChunk charsPerDot dot) chunks) 0

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

-- | Add bracketing chunks displaying the command and its arguments at
-- the beginning, and the result code at the end.
insertCommandStart :: (IsString a, ListLikePlus a c, Eq c) => CreateProcess -> [Chunk a] -> [Chunk a]
insertCommandStart p chunks =
    [Stderr (fromString (" -> " ++ displayCreateProcess p ++ "\n"))] ++
    Prelude.concatMap
      (foldChunk ((: []) . ProcessHandle)
                 ((: []) . Stdout)
                 ((: []) . Stderr)
                 ((: []) . Exception)
                 ((: []) . Result)) chunks

-- | Add bracketing chunks displaying the command and its arguments at
-- the beginning, and the result code at the end.
insertCommandResult :: (IsString a, ListLikePlus a c, Eq c) => CreateProcess -> [Chunk a] -> [Chunk a]
insertCommandResult p chunks =
    Prelude.concatMap
      (foldChunk ((: []) . ProcessHandle)
                 ((: []) . Stdout)
                 ((: []) . Stderr)
                 ((: []) . Exception)
                 (\ code -> [Stderr (fromString (" <- " ++ show code ++ " <- " ++ showCmdSpecForUser (cmdspec p) ++ "\n")), Result code])) chunks

insertCommandDisplay :: (IsString a, ListLikePlus a c, Eq c) => CreateProcess -> [Chunk a] -> [Chunk a]
insertCommandDisplay p = insertCommandResult p . insertCommandStart p

eraseStdout :: Monoid a => [Chunk a] -> [Chunk a]
eraseStdout = Prelude.map (foldChunk ProcessHandle
                                     (\ _ -> Stdout mempty)
                                     Stderr
                                     Exception
                                     Result)

eraseStderr :: Monoid a => [Chunk a] -> [Chunk a]
eraseStderr = Prelude.map (foldChunk ProcessHandle
                                     Stdout
                                     (\ _ -> Stderr mempty)
                                     Exception
                                     Result)

eraseOutput :: Monoid a => [Chunk a] -> [Chunk a]
eraseOutput = Prelude.map (foldChunk ProcessHandle
                                     (\ _ -> Stdout mempty)
                                     (\ _ -> Stderr mempty)
                                     Exception
                                     Result)

mergeToStdout :: [Chunk a] -> [Chunk a]
mergeToStdout = Prelude.map (foldChunk ProcessHandle Stdout Stdout Exception Result)

mergeToStderr :: [Chunk a] -> [Chunk a]
mergeToStderr = Prelude.map (foldChunk ProcessHandle Stderr Stderr Exception Result)

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
                   id
                   (\ _ -> mempty)
                   (\ _ -> mempty)
                   (\ _ -> mempty)) chunks

collectOutputAndError :: ListLike a c => [Chunk a] -> a
collectOutputAndError chunks =
    mconcat $ Prelude.map
                (foldChunk
                   (\ _ -> mempty)
                   id
                   id
                   (\ _ -> mempty)
                   (\ _ -> mempty)) chunks

collectExitCode :: ListLike a c => [Chunk a] -> ExitCode
collectExitCode chunks =
    mconcat $ Prelude.map
                (foldChunk
                   (\ _ -> mempty)
                   (\ _ -> mempty)
                   (\ _ -> mempty)
                   (\ _ -> mempty)
                   id) chunks

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
-- in the input stream, replacing the Exception chunk with the return
-- value of f.  Or, f itself might throw an exception.
withProcessException :: (ListLike a c, Monad m) => (SomeException -> m (Chunk a)) -> [Chunk a] -> m [Chunk a]
withProcessException f input =
    mapM (foldChunk
            (return . ProcessHandle)
            (return . Stdout)
            (return . Stderr)
            f
            (return . Result)) input

-- | Write the Stdout chunks to stdout and the Stderr chunks to stderr.
putChunk :: ListLikePlus a c => Chunk a -> IO ()
putChunk (Stdout x) = putStr x
putChunk (Stderr x) = hPutStr stderr x
putChunk _ = return ()

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

-- | Turn the standard output of one process into the input of
-- another, throwing an IO exception if the input process result code
-- is ExitFailure.
pipeProcessChunks :: (ListLikePlus a c) => CreateProcess -> [Chunk a] -> IO [Chunk a]
pipeProcessChunks p input = collectProcessOutput' p input >>= readProcessChunks p

-- | A version of collectProcessOutput which throws an exception of
-- the exit code is ExitFailure.
collectProcessOutput' :: ListLike a c => CreateProcess -> [Chunk a] -> IO a
collectProcessOutput' p chunks =
    withProcessResult (throwProcessResult "collectProcessOutput'" (showCmdSpecForUser (cmdspec p))) chunks >>= return . collectProcessOutput

collectOutputAndError' :: ListLike a c => CreateProcess -> [Chunk a] -> IO a
collectOutputAndError' p chunks =
    withProcessResult (throwProcessResult "collectOutputAndError'" (showCmdSpecForUser (cmdspec p))) chunks >>= return . collectOutputAndError

throwExitCode :: ListLike a c => CreateProcess -> [Chunk a] -> IO ()
throwExitCode p chunks =
    void $ throwProcessResult "throwExitCode" (showCmdSpecForUser (cmdspec p)) (collectExitCode chunks)

-- | Apply the function to the chunk list and output the result,
-- return the original (unmapped) chunk list.
putMappedChunks :: ListLikePlus a c => ([Chunk a] -> [Chunk a]) -> [Chunk a] -> IO [Chunk a]
putMappedChunks f chunks = mapM_ putChunk (f chunks) >> return chunks

putIndentedShowCommand :: (ListLikePlus a c, Eq c, IsString a) =>
                          CreateProcess -> String -> String -> [Chunk a] -> IO [Chunk a]
putIndentedShowCommand p outp errp chunks =
    putMappedChunks (insertCommandDisplay p . indentChunks outp errp) chunks

-- | Output the indented text of a chunk list, but return the original
-- unindented list.  Returns the original chunks.
putIndented :: (ListLikePlus a c, Eq c, IsString a) => String -> String -> [Chunk a] -> IO [Chunk a]
putIndented outp errp chunks = putMappedChunks (indentChunks outp errp) chunks

-- | Output the dotified text of a chunk list, but return the original
-- unindented list.
putDots :: (ListLikePlus a c) => Int -> c -> [Chunk a] -> IO [Chunk a]
putDots charsPerDot dot chunks =
    evalStateT (mapM (\ x -> dotifyChunk charsPerDot dot x >>= mapM_ (lift . putChunk) >> return x) chunks) 0

putDotsLn :: forall a c. (IsString a, ListLikePlus a c) => Int -> c -> [Chunk a] -> IO [Chunk a]
putDotsLn cpd dot chunks = putDots cpd dot chunks >>= \ r -> hPutStr stderr (fromString "\n" :: a) >> return r

displayCreateProcess :: CreateProcess -> String
displayCreateProcess p = showCmdSpecForUser (cmdspec p) ++ maybe "" (\ d -> " (in " ++ d ++ ")") (cwd p)
