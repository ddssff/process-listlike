-- | The hhGetContents function reads the output from two handles
-- (presumably stdout and stderr) and interleaves them into a list of
-- Output.  Unlike readProcessWithExitCode, this preserves the order
-- in which the chunks of text were written by the process.

{-# LANGUAGE ScopedTypeVariables #-}
module System.Process.Read.Chunks (
  NonBlocking(..),
  Output(..),
  readProcessChunks,
  isResult,
  isStdout,
  isStderr,
  isException,
  discardStdout,
  discardStderr,
  discardExceptions,
  discardResult,
  keepStdout,
  keepStderr,
  keepExceptions,
  keepResult,
  mergeToStdout,
  mergeToStderr,
  sendOutputs,
  foldChunks,
  foldChunks',
  dots
  ) where

import Control.Applicative (Applicative, (<$>))
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Int (Int64)
import Data.Monoid (Monoid, mconcat)
import qualified GHC.IO.Exception as E
import Prelude hiding (catch, null, length, init, rem)
import System.Exit (ExitCode, exitWith)
import System.IO hiding (hPutStr, hGetContents)
import System.Process (CreateProcess(..), StdStream(CreatePipe),
                       CmdSpec, createProcess, waitForProcess, terminateProcess)
import System.Process.Read.Chars (Chars(init, null, hPutStr, length), proc', forkWait, resourceVanished)

-- | Class of types which can also be used by the hhGetContents function
class Chars a => NonBlocking a where
  hGetNonBlocking :: Handle -> Int -> IO a

data Output a = Stdout a | Stderr a | Result ExitCode | Exception IOError deriving Show
data Readyness = Ready | Unready | EndOfFile

-- | This is a process runner which (at the cost of a busy loop) gives
-- you the chunks of text read from stdout and stderr interleaved in
-- the order they were written, along with any ResourceVanished
-- exxception that might occur.  Its interface is similar to
-- 'System.Process.Read.readModifiedProcessWithExitCode', though the
-- implementation is somewhat alarming.
readProcessChunks
    :: NonBlocking a =>
       (CreateProcess -> CreateProcess)
                                -- ^ Modify CreateProcess with this
    -> CmdSpec                  -- ^ command to run
    -> a                        -- ^ standard input
    -> IO [Output a]
readProcessChunks modify cmd input = mask $ \restore -> do
    let modify' p = (modify p) {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }

    (Just inh, Just outh, Just errh, pid) <-
        createProcess (modify' (proc' cmd))

    init input [inh, outh, errh]

    flip onException
      (do hClose inh; hClose outh; hClose errh;
          terminateProcess pid; waitForProcess pid) $ restore $ do

      -- fork off one thread to start consuming both stdout and stderr
      waitOut <- forkWait $ hhGetContents outh errh

      -- now write and flush any input
      exns <- if null input
              then return []
              else (hPutStr inh input >> hFlush inh >> hClose inh >> return []) `catch` resourceVanished (return . (: []))

      -- wait on the output
      outs <- waitOut

      hClose outh
      hClose errh

      -- wait on the process
      ex <- waitForProcess pid

      return (outs {- ++ map Exception exns ++ [Result ex] -})

bufSize :: Int
bufSize = 4096		-- maximum chunk size
uSecs :: Int
uSecs = 8		-- minimum wait time, doubles each time nothing is ready
maxUSecs :: Int
maxUSecs = 100000	-- maximum wait time (microseconds)

-- | Read from two handles and interleave the output.
hhGetContents :: NonBlocking a => Handle -> Handle -> IO [Output a]
hhGetContents outh errh = hhGetContents' (Just outh, Just errh, [])

hhGetContents' :: NonBlocking a => (Maybe Handle, Maybe Handle, [Output a]) -> IO [Output a]
hhGetContents' (Nothing, Nothing, outputs) = return outputs
hhGetContents' (outh, errh, outputs) =
    hhGetContents'' uSecs >>= hhGetContents'
    where
      hhGetContents'' waitUSecs =
          do outReady <- maybe (return Unready) hReady' outh
             errReady <- maybe (return Unready) hReady' errh
             case (outReady, errReady) of
               (Unready, Unready) ->
                   do -- Neither output handle is ready, wait a bit.
                      -- Initial wait is 8 usecs, increasing to 0.1
                      -- sec.
                      threadDelay waitUSecs
                      hhGetContents'' (min maxUSecs (2 * waitUSecs))
               _ ->
                   do -- At least one handle is ready. Do a read.
                      (err, errh') <- nextOut errh errReady Stderr
                      (out, outh') <- nextOut outh outReady Stdout
                      return (outh', errh', outputs ++ err ++ out)

-- | Return the next output chunk and the updated handle from a handle.
nextOut :: NonBlocking a => (Maybe Handle) -> Readyness -> (a -> Output a) -> IO ([Output a], Maybe Handle)
nextOut Nothing _ _ = return ([], Nothing)	-- Handle is closed
nextOut _ EndOfFile _ = return ([], Nothing)	-- Handle is closed
nextOut h Unready _ = return ([], h)	-- Handle is not ready
nextOut (Just h) Ready constructor =	-- Perform a read.
    do
      a <- hGetNonBlocking h bufSize
      case length a of
        -- A zero length read, unlike a zero length write, always
        -- means EOF.
        0 -> do hClose h
                return ([], Nothing)
        -- Got some input
        _n -> return ([constructor a], Just h)

-- | Determine whether a handle's state is Ready, Unready, or EndOfFile.
hReady' :: Handle -> IO Readyness
hReady' h =
  readyness `catch` handleEOF
  where
    readyness = hReady h >>= \ flag -> return $ if flag then Ready else Unready
    handleEOF e | E.ioe_type e == E.EOF = return EndOfFile
    handleEOF e = throw e

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

stdoutText :: Output a -> Maybe a
stdoutText (Stdout x) = Just x
stdoutText _ = Nothing

stderrText :: Output a -> Maybe a
stderrText (Stderr x) = Just x
stderrText _ = Nothing
