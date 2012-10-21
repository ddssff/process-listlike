{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module System.Process.Read.Convenience
    ( -- * Predicates
      isResult
    , isStdout
    , isStderr
    , isOutput
    , isException
    -- * Filters
    , discardStdout
    , discardStderr
    , discardOutput
    , discardExceptions
    , discardResult
    , keepStdout
    , keepStderr
    , keepOutput
    , keepExceptions
    , keepResult
    -- * Transformers
    , mergeToStdout
    , mergeToStderr
    , mapMaybeResult
    , mapMaybeStdout
    , mapMaybeStderr
    , mapMaybeException
    , unpackOutputs
    -- * IO operations
    , ePutStr
    , ePutStrLn
    , foldException
    , foldChars
    , foldStdout
    , foldStderr
    , foldExit
    , foldSuccess
    , foldFailure

    , doException
    , doOutput
    , doStdout
    , doStderr
    , doExit
    , doAll

    , dots
    ) where

import Control.Applicative ((<$>))
import Control.Exception (throw)
import Control.Monad.State (StateT(runStateT), get, put)
import Control.Monad.Trans (lift, MonadIO, liftIO)
import Data.Maybe (mapMaybe)
import Prelude hiding (length, rem, concat)
import System.Exit (ExitCode(..), exitWith)
import System.IO (stdout, stderr)
import qualified System.IO as IO (hPutStr, hPutStrLn)
import System.Process.Read.Chars (Chars(..))
import System.Process.Read.Chunks (NonBlocking(..), Output(..), foldOutput, foldOutputs)

isResult :: Chars a => Output a -> Bool
isResult = foldOutput (const True) (const False) (const False) (const False)
isStdout :: Chars a => Output a -> Bool
isStdout = foldOutput (const False) (const True) (const False) (const False)
isStderr :: Chars a => Output a -> Bool
isStderr = foldOutput (const False) (const False) (const True) (const False)
isOutput :: Chars a => Output a -> Bool
isOutput = foldOutput (const False) (const True) (const True) (const False)
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
discardOutput :: Chars a => [Output a] -> [Output a]
discardOutput = filter (\ x -> not (isStdout x || isStderr x))
discardExceptions :: Chars a => [Output a] -> [Output a]
discardExceptions = filter (not . isException)
discardResult :: Chars a => [Output a] -> [Output a]
discardResult = filter (not . isResult)

keepStdout :: Chars a => [Output a] -> [a]
keepStdout = mapMaybe $ foldOutput (const Nothing) Just (const Nothing) (const Nothing)
keepStderr :: Chars a => [Output a] -> [a]
keepStderr = mapMaybe $ foldOutput (const Nothing) (const Nothing) Just (const Nothing)
keepOutput :: Chars a => [Output a] -> [a]
keepOutput = mapMaybe $ foldOutput (const Nothing) Just Just (const Nothing)
keepExceptions :: Chars a => [Output a] -> [IOError]
keepExceptions = mapMaybe $ foldOutput (const Nothing) (const Nothing) (const Nothing) Just
keepResult :: Chars a => [Output a] -> [ExitCode]
keepResult = mapMaybe $ foldOutput Just (const Nothing) (const Nothing) (const Nothing)

mapMaybeResult :: Chars a => (ExitCode -> Maybe (Output a)) -> [Output a] -> [Output a]
mapMaybeResult f = mapMaybe (foldOutput f (Just . Stdout) (Just . Stderr) (Just . Exception))
mapMaybeStdout :: Chars a => (a -> Maybe (Output a)) -> [Output a] -> [Output a]
mapMaybeStdout f = mapMaybe (foldOutput (Just . Result) f (Just . Stderr) (Just . Exception))
mapMaybeStderr :: Chars a => (a -> Maybe (Output a)) -> [Output a] -> [Output a]
mapMaybeStderr f = mapMaybe (foldOutput (Just . Result) (Just . Stdout) f (Just . Exception))
mapMaybeException :: Chars a => (IOError -> Maybe (Output a)) -> [Output a] -> [Output a]
mapMaybeException f = mapMaybe (foldOutput (Just . Result) (Just . Stdout) (Just . Stderr) f)

unpackOutputs :: forall a. Chars a => [Output a] -> ([ExitCode], String, String, [IOError])
unpackOutputs xs =
    (codes, toString outs, toString errs, exns)
    where
      (codes, outs, errs, exns) = foldOutputs codefn outfn errfn exnfn result0 xs
      result0 :: ([ExitCode], a, a, [IOError])
      result0 = ([], empty, empty, [])
      codefn :: ([ExitCode], a, a, [IOError]) -> ExitCode -> ([ExitCode], a, a, [IOError])
      codefn (codes, outs, errs, exns) code = (code : codes, outs, errs, exns)
      outfn :: ([ExitCode], a, a, [IOError]) -> a -> ([ExitCode], a, a, [IOError])
      outfn (codes, outs, errs, exns) out = (codes, append out outs, errs, exns)
      errfn :: ([ExitCode], a, a, [IOError]) -> a -> ([ExitCode], a, a, [IOError])
      errfn (codes, outs, errs, exns) err = (codes, outs, append err errs, exns)
      exnfn :: ([ExitCode], a, a, [IOError]) -> IOError -> ([ExitCode], a, a, [IOError])
      exnfn (codes, outs, errs, exns) exn = (codes, outs, errs, exn : exns)

ePutStr :: MonadIO m => String -> m ()
ePutStr s = liftIO $ IO.hPutStr stderr s

ePutStrLn :: MonadIO m => String -> m ()
ePutStrLn s = liftIO $ IO.hPutStrLn stderr s

foldException :: Chars a => (IOError -> IO (Output a)) -> [Output a] -> IO [Output a]
foldException exnfn = mapM (foldOutput (return . Result) (return . Stdout) (return . Stderr) exnfn)

foldChars :: Chars a => (a -> IO (Output a)) -> (a -> IO (Output a)) -> [Output a] -> IO [Output a]
foldChars outfn errfn = mapM (foldOutput (return . Result) outfn errfn (return . Exception))

foldStdout :: Chars a => (a -> IO (Output a)) -> [Output a] -> IO [Output a]
foldStdout outfn = foldChars outfn (return . Stderr)

foldStderr :: Chars a => (a -> IO (Output a)) -> [Output a] -> IO [Output a]
foldStderr errfn = foldChars (return . Stdout) errfn

foldExit :: Chars a => (ExitCode -> IO (Output a)) -> [Output a] -> IO [Output a]
foldExit codefn = mapM (foldOutput codefn (return . Stdout) (return . Stderr) (return . Exception))

foldFailure :: Chars a => (Int -> IO (Output a)) -> [Output a] -> IO [Output a]
foldFailure failfn = foldExit codefn
    where codefn (ExitFailure n) = failfn n
          codefn x = return (Result x)

foldSuccess :: Chars a => IO (Output a) -> [Output a] -> IO [Output a]
foldSuccess successfn = foldExit codefn
    where codefn ExitSuccess = successfn
          codefn x = return (Result x)

doException :: Chars a => [Output a] -> IO [Output a]
doException = foldException throw

doOutput :: Chars a => [Output a] -> IO [Output a]
doOutput = foldChars (\ cs -> hPutStr stdout cs >> return (Stdout cs)) (\ cs -> hPutStr stderr cs >> return (Stderr cs))

doStdout :: Chars a => [Output a] -> IO [Output a]
doStdout = foldStdout (\ cs -> hPutStr stdout cs >> return (Stdout cs))

doStderr :: Chars a => [Output a] -> IO [Output a]
doStderr = foldStderr (\ cs -> hPutStr stderr cs >> return (Stderr cs))

-- | I don't see much use for this.
doExit :: Chars a => [Output a] -> IO [Output a]
doExit = foldExit (\ code -> exitWith code >> return (Result code))

doAll :: Chars a => [Output a] -> IO [Output a]
doAll = mapM (foldOutput (\ code -> exitWith code >> return (Result code))
                         (\ cs -> hPutStr stdout cs >> return (Stdout cs))
                         (\ cs -> hPutStr stderr cs >> return (Stderr cs))
                         throw)

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
