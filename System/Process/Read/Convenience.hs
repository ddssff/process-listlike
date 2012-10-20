{-# LANGUAGE RankNTypes #-}
module System.Process.Read.Convenience
    ( -- * Predicates
      isResult
    , isStdout
    , isStderr
    , isException
    -- * Filters
    , discardStdout
    , discardStderr
    , discardExceptions
    , discardResult
    , keepStdout
    , keepStderr
    , keepExceptions
    , keepResult
    -- * Transformers
    , mergeToStdout
    , mergeToStderr
    , mapMaybeResult
    , mapMaybeStdout
    , mapMaybeStderr
    , mapMaybeException
    -- * IO operations
    , ePutStr
    , ePutStrLn
    , doOutput
    , doOutputs
    , doOutputs_
    , dots
    ) where

import Control.Applicative ((<$>))
import Control.Exception (throw)
import Control.Monad.State (StateT(runStateT), get, put)
import Control.Monad.Trans (lift, MonadIO, liftIO)
import Data.Maybe (mapMaybe)
import Prelude hiding (length, rem)
import System.Exit (ExitCode, exitWith)
import System.IO (stdout, stderr)
import qualified System.IO as IO (hPutStr, hPutStrLn)
import System.Process.Read.Chars (Chars(..))
import System.Process.Read.Chunks (NonBlocking(..), Output(..), foldOutput)

isResult :: Chars a => Output a -> Bool
isResult = foldOutput (const True) (const False) (const False) (const False)
isStdout :: Chars a => Output a -> Bool
isStdout = foldOutput (const False) (const True) (const False) (const False)
isStderr :: Chars a => Output a -> Bool
isStderr = foldOutput (const False) (const False) (const True) (const False)
isException :: Chars a => Output a -> Bool
isException = foldOutput (const False) (const False) (const False) (const True)

toStdout :: Chars a => Output a -> Output a
toStdout = foldOutput Result Stdout Stdout Exception
toStderr :: Chars a => Output a -> Output a
toStderr = foldOutput Result Stderr Stderr Exception

mergeToStdout :: Chars a => [Output a] -> [Output a]
mergeToStdout = map toStdout
mergeToStderr :: Chars a => [Output a] -> [Output a]
mergeToStderr = map toStderr

discardStdout :: Chars a => [Output a] -> [Output a]
discardStdout = filter (not . isStdout)
discardStderr :: Chars a => [Output a] -> [Output a]
discardStderr = filter (not . isStderr)
discardExceptions :: Chars a => [Output a] -> [Output a]
discardExceptions = filter (not . isException)
discardResult :: Chars a => [Output a] -> [Output a]
discardResult = filter (not . isResult)

keepStdout :: Chars a => [Output a] -> [Output a]
keepStdout = filter isStdout
keepStderr :: Chars a => [Output a] -> [Output a]
keepStderr = filter isStderr
keepExceptions :: Chars a => [Output a] -> [Output a]
keepExceptions = filter isException
keepResult :: Chars a => [Output a] -> [Output a]
keepResult = filter isResult

mapMaybeResult :: Chars a => (ExitCode -> Maybe (Output a)) -> [Output a] -> [Output a]
mapMaybeResult f = mapMaybe (foldOutput f (Just . Stdout) (Just . Stderr) (Just . Exception))
mapMaybeStdout :: Chars a => (a -> Maybe (Output a)) -> [Output a] -> [Output a]
mapMaybeStdout f = mapMaybe (foldOutput (Just . Result) f (Just . Stderr) (Just . Exception))
mapMaybeStderr :: Chars a => (a -> Maybe (Output a)) -> [Output a] -> [Output a]
mapMaybeStderr f = mapMaybe (foldOutput (Just . Result) (Just . Stdout) f (Just . Exception))
mapMaybeException :: Chars a => (IOError -> Maybe (Output a)) -> [Output a] -> [Output a]
mapMaybeException f = mapMaybe (foldOutput (Just . Result) (Just . Stdout) (Just . Stderr) f)

ePutStr :: MonadIO m => String -> m ()
ePutStr s = liftIO $ IO.hPutStr stderr s

ePutStrLn :: MonadIO m => String -> m ()
ePutStrLn s = liftIO $ IO.hPutStrLn stderr s

doOutput :: Chars a => Output a -> IO ()
doOutput = foldOutput exitWith (hPutStr stdout) (hPutStr stderr) throw

doOutputs :: NonBlocking a => [Output a] -> IO [Output a]
doOutputs = mapM (\ x -> doOutput x >> return x)

doOutputs_ :: NonBlocking a => [Output a] -> IO ()
doOutputs_ = mapM_ doOutput

dots :: forall a. NonBlocking a => LengthType a -> (LengthType a -> IO ()) -> [Output a] -> IO [Output a]
dots charsPerDot nDots outputs =
    fst <$> runStateT (dots' outputs) 0
    where
      dots' [] = return []
      dots' (x : xs) = do
          rem <- get
          let (count', rem') = divMod (rem + foldOutput (const 0) length length (const 0) x) charsPerDot
          lift (nDots count')
          put rem'
          dots' xs >>= \ xs' -> return (x : xs')
