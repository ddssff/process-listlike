-- | Some functions brought over from my obsolete progress packages.
{-# LANGUAGE RankNTypes, ScopedTypeVariables, TypeFamilies #-}
module System.Process.Read.Compat
    ( prefixes
    , echo
    , doOutput'
    , lazyCommandE
    , lazyCommandEF
    , lazyCommandF
    , lazyCommandV
    , lazyCommand
    , oneResult
    , quieter
    , quieter'
    , q12
    , qPutStrLn
    , qPutStr
    , timeTask
    ) where

import Control.Exception (evaluate)
import Control.Monad.Trans (MonadIO)
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Time (NominalDiffTime, getCurrentTime, diffUTCTime)
import System.Exit (ExitCode(..))
import System.Process (CmdSpec(..), showCommandForUser)
import System.Process.Read.Chars (Chars)
import System.Process.Read.Convenience (ePutStrLn, doOutput, foldFailure, keepResult)
import System.Process.Read.Chunks (NonBlocking, Output(..), readProcessChunks)

-- | Return the original stream of outputs zipped with one that has
-- had prefixes for stdout and stderr inserted.  For the prefixed
-- stream only, apply @map snd@.
prefixes :: forall a. (a ~ L.ByteString) => a -> a -> [Output a] -> [(Output a, Output a)]
prefixes opre epre output =
    f True output
    where
      f :: Bool -> [Output a] -> [(Output a, Output a)]
      f _ [] = []
      f bol (x@(Stdout s) : xs) = let (s', bol') = doOutput' bol opre s in (x, Stdout s') : f bol' xs
      f bol (x@(Stderr s) : xs) = let (s', bol') = doOutput' bol epre s in (x, Stderr s') : f bol' xs
      f bol (x : xs) = (x, Stdout L.empty) : f bol xs

doOutput' :: forall a. (a ~ L.ByteString) => Bool -> a -> a -> (a, Bool)
doOutput' bol pre s =
    let (a, b) = L.span (/= '\n') s in
    if L.null a
    then if L.null b
         then (L.empty, bol)
         else let x = (if bol then pre else L.empty)
                  (s', bol') = doOutput' True pre (L.tail b) in
              (L.concat [x, (L.pack "\n"), s'], bol')
          -- There is some text before a possible newline
    else let x = (if bol then L.append pre a else a)
             (s', bol') = doOutput' False pre b in
         (L.append x s', bol')

echo :: CmdSpec -> IO () -> IO ()
echo (RawCommand cmd args) io = ePutStrLn ("-> " ++ showCommandForUser cmd args) >> io
echo (ShellCommand cmd) io = ePutStrLn ("-> " ++ cmd) >> io

lazyCommandE :: NonBlocking a => String -> a -> IO [Output a]
lazyCommandE cmd input = readProcessChunks id (ShellCommand cmd) input >>= doOutput
lazyCommandEF :: NonBlocking a => String -> a -> IO [Output a]
lazyCommandEF cmd input = readProcessChunks id (ShellCommand cmd) input >>= doOutput >>= foldFailure (\ n -> error ("Command failed with exit code " ++ show n))
lazyCommandF :: NonBlocking a => String -> a -> IO [Output a]
lazyCommandF cmd input = readProcessChunks id (ShellCommand cmd) input >>= doOutput >>= foldFailure (\ n -> error ("Command failed with exit code " ++ show n))
lazyCommandV :: NonBlocking a => String -> a -> IO [Output a]
lazyCommandV cmd input = readProcessChunks id (ShellCommand cmd) input >>= doOutput
-- lazyCommandP cmd input = readProcessChunks id (ShellCommand cmd) input >>= doOutput >>= doException

oneResult :: Chars a => [Output a] -> ExitCode
oneResult xs =
    case keepResult xs of
      [code] -> code
      [] -> error "Missing result code"
      codes -> error $ "Multiple result codes: " ++ show codes

{-
doFailure :: Chars a => (Int -> IO ()) -> [Output a] -> IO [Output a]
doFailure onFailure = mapM (\ x -> foldOutput doFailure ignore ignore ignore x >> return x)
    where doFailure ExitSuccess = return ()
          doFailure (ExitFailure n) = onFailure n

checkResult :: (Int -> IO ()) -> [Output L.ByteString] -> IO [Output L.ByteString]
checkResult = doFailure
-}

quieter' :: (Int -> Int) -> a -> a
quieter' = const id

quieter :: (Int -> Int) -> a -> a
quieter = const id

q12 :: MonadIO m => String -> m a -> m a
q12 msg x = qPutStrLn msg >> x

qPutStrLn :: MonadIO m => String -> m ()
qPutStrLn = ePutStrLn

qPutStr :: MonadIO m => String -> m ()
qPutStr = ePutStrLn

-- |Run a task and return the elapsed time along with its result.
timeTask :: IO a -> IO (a, NominalDiffTime)
timeTask x =
    do start <- getCurrentTime
       result <- x >>= evaluate
       finish <- getCurrentTime
       return (result, diffUTCTime finish start)

lazyCommand :: NonBlocking a => String -> a -> IO [Output a]
lazyCommand cmd input = readProcessChunks id (ShellCommand cmd) input
