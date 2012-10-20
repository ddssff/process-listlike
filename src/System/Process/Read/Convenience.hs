{-# LANGUAGE RankNTypes #-}
module System.Process.Read.Convenience
    ( isResult
    , isStdout
    , isStderr
    , isException
    , discardStdout
    , discardStderr
    , discardExceptions
    , discardResult
    , keepStdout
    , keepStderr
    , keepExceptions
    , keepResult
    , mergeToStdout
    , mergeToStderr
    , sendOutputs
    , foldChunks
    , foldChunks'
    , dots
    , mapMaybeStdout
    , stdoutText
    , stderrText
    ) where

import Control.Applicative (Applicative, (<$>))
import Control.Exception (throw)
import Data.Monoid (Monoid, mconcat)
import Prelude hiding (length, rem)
import System.Exit (ExitCode, exitWith)
import System.IO (stdout, stderr)
import System.Process.Read.Chars (Chars(..))
import System.Process.Read.Chunks (NonBlocking(..), Output(..))

discardStdout :: [Output a] -> [Output a]
discardStdout = filter (not . isStdout)
discardStderr :: [Output a] -> [Output a]
discardStderr = filter (not . isStderr)
discardExceptions :: [Output a] -> [Output a]
discardExceptions = filter (not . isException)
discardResult :: [Output a] -> [Output a]
discardResult = filter (not . isResult)

mergeToStdout :: [Output a] -> [Output a]
mergeToStdout = map toStdout
mergeToStderr :: [Output a] -> [Output a]
mergeToStderr = map toStderr

toStdout :: Output a -> Output a
toStdout (Stderr s) = Stdout s
toStdout x = x
toStderr :: Output a -> Output a
toStderr (Stdout s) = Stderr s
toStderr x = x

keepStdout :: [Output a] -> [Output a]
keepStdout = filter isStdout
keepStderr :: [Output a] -> [Output a]
keepStderr = filter isStderr
keepExceptions :: [Output a] -> [Output a]
keepExceptions = filter isException
keepResult :: [Output a] -> [Output a]
keepResult = filter isResult

isStdout :: Output a -> Bool
isStdout (Stdout _) = True
isStdout _ = False
isStderr :: Output a -> Bool
isStderr (Stderr _) = True
isStderr _ = False
isException :: Output a -> Bool
isException (Exception _) = True
isException _ = False
isResult :: Output a -> Bool
isResult (Result _) = True
isResult _ = False

foldChunks :: (Monad m, Applicative m, Chars a) =>
              (ExitCode -> m b)
           -> (a -> m b)
           -> (a -> m b)
           -> (IOError -> m b)
           -> [Output a]
           -> m [b]
foldChunks codefn outfn errfn exnfn xs =
    mapM f xs
    where
      f (Result code) = codefn code
      f (Stdout out) = outfn out
      f (Stderr err) = errfn err
      f (Exception exn) = exnfn exn

foldChunks' :: (Monoid b, Monad m, Applicative m, Chars a) =>
              (ExitCode -> m b)
            -> (a -> m b)
            -> (a -> m b)
            -> (IOError -> m b)
            -> [Output a]
            -> m b
foldChunks' codefn outfn errfn exnfn xs =
    mconcat <$> foldChunks codefn outfn errfn exnfn xs

sendOutputs :: NonBlocking a => [Output a] -> IO ()
sendOutputs = foldChunks' exitWith (hPutStr stdout) (hPutStr stderr) throw

dots :: forall a. NonBlocking a => Int -> (Int -> IO ()) -> [Output a] -> IO [Output a]
dots charsPerDot put outputs =
    put 1 >> dots' 0 outputs >> return outputs
    where
      dots' :: NonBlocking a => Int -> [Output a] -> IO ()
      dots' _ [] = return ()
      dots' rem (x : more) =
          let count = case x of
                        Stdout s -> length s
                        Stderr s -> length s
                        _ -> 0
              (count', rem') = divMod (rem + fromInteger (toInteger count)) charsPerDot in
          put count' >> dots' rem' more 

mapMaybeStdout :: (a -> Maybe (Output a)) -> [Output a] -> [Output a]
mapMaybeStdout f (Stdout s : more) = maybe [] (: []) ( f s) ++ mapMaybeStdout f more
mapMaybeStdout f (x : more) = x : mapMaybeStdout f more
mapMaybeStdout _ [] = []

stdoutText :: Output a -> Maybe a
stdoutText (Stdout x) = Just x
stdoutText _ = Nothing

stderrText :: Output a -> Maybe a
stderrText (Stderr x) = Just x
stderrText _ = Nothing
