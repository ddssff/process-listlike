-- | A perhaps over-engineered set of wrappers around
-- readProcessChunks to run processes with a variety of echoing
-- options and responses to failure.
{-# LANGUAGE FlexibleInstances, RankNTypes, ScopedTypeVariables, TypeFamilies, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module System.Process.Read.Monad
    ( -- * Run processes with various types and amounts of feedback
      runProcessQ
    , runProcessD
    , runProcessV
    , runProcessQF
    , runProcessDF
    , runProcessVF
    , runProcessQE
    , runProcessDE
    -- * Process feedback managed by the VERBOSITY environment variable
    , runProcess
    , runProcessF
    ) where

import Control.Monad (when)
import Control.Monad.State (StateT(runStateT), get, put)
import Control.Monad.Trans (MonadIO, liftIO)
import Prelude hiding (print)
import System.Exit (ExitCode(ExitFailure))
import System.IO (hPutStrLn, stderr)
import System.Process (CreateProcess, CmdSpec(RawCommand, ShellCommand), showCommandForUser)
import qualified System.Process.Read.Chars as P
import qualified System.Process.Read.Chunks as P
import qualified System.Process.Read.Convenience as P
import System.Process.Read.Verbosity (verbosity)

-- | The state we need when running processes
data RunState c
    = RunState
      { cpd :: Int  -- Output one dot per n characters of process output, 0 means no dots
      , trace :: Bool -- Echo the command line before starting, and after with the result code
      , echo :: Bool -- Echo the process output to the console
      , failEcho :: Bool -- Echo the process output if the result code is ExitFailure
      , failExit :: Bool -- Throw an IOError if the result code is ExitFailure
      , prefixes :: Maybe (String, String) -- Prepend a prefix to the echoed lines of stdout and stderr
      }

defaultRunState :: P.Chars c => RunState c
defaultRunState = RunState {cpd=0, trace=True, echo=False, failEcho=False, failExit=False, prefixes=Nothing}

-- | The monad for running processes
type RunT c = StateT (RunState c)

withRunState :: MonadIO m => RunState c -> RunT c m a -> m a
withRunState s action =
    (runStateT action) s >>= return . fst

modifyRunState :: MonadIO m => (RunState c -> RunState c) -> RunT c m ()
modifyRunState modify = get >>= put . modify

dotsPerChar :: MonadIO m => Int -> RunT c m ()
dotsPerChar x = modifyRunState (\ (RunState _ b c d e f) -> RunState x b c d e f) >> echoOutput False

echoCommand :: MonadIO m => Bool -> RunT c m ()
echoCommand x = modifyRunState (\ (RunState a _ c d e f) -> RunState a x c d e f)

echoOutput :: MonadIO m => Bool -> RunT c m ()
echoOutput x = modifyRunState (\ (RunState a b _ d e f) -> RunState a b x d e f) >> echoOnFailure False

echoOnFailure :: MonadIO m => Bool -> RunT c m ()
echoOnFailure x = modifyRunState (\ (RunState a b c _ e f) -> RunState a b c x e f) >> exceptionOnFailure True

exceptionOnFailure :: MonadIO m => Bool -> RunT c m ()
exceptionOnFailure x = modifyRunState (\ (RunState a b c d _ f) -> RunState a b c d x f)

setPrefixes :: MonadIO m => Maybe (String, String) -> RunT c m ()
setPrefixes x = modifyRunState (\ (RunState a b c d e _) -> RunState a b c d e x)

runProcessM :: (P.NonBlocking c, MonadIO m) => (CreateProcess -> CreateProcess) -> CmdSpec -> c -> RunT c m [P.Output c]
runProcessM f cmd input =
    do s <- get
       liftIO $ do
         when (trace s) (hPutStrLn stderr ("-> " ++ showCommand cmd))
         (out1 :: [P.Output c]) <- P.readProcessChunks f cmd input
         (out2 :: [P.Output c]) <- maybe (return out1) (\ (sout, serr) -> P.prefixed sout serr out1) (prefixes s)
         (out3 :: [P.Output c]) <- (if echo s then P.doOutput else return) out2
         (out4 :: [P.Output c]) <- if cpd s > 0 then P.dots (fromIntegral (cpd s)) (\ n -> P.hPutStr stderr (replicate (fromIntegral n) '.')) out3 else return out3
         (out5 :: [P.Output c]) <- (if failExit s then P.foldFailure (\ n -> error (showCommand cmd ++ " -> ExitFailure " ++ show n)) else return) out4
         (out6 :: [P.Output c]) <- (if failEcho s then P.foldFailure (\ n -> P.doOutput out5 >> return (P.Result (ExitFailure n))) else return) out5
         (out7 :: [P.Output c]) <- (if trace s then  P.foldResult (\ ec -> hPutStrLn stderr ("<- " ++ showCommand cmd ++ ": " ++ show ec) >> return (P.Result ec)) else return) out6
         return out7

c :: MonadIO m => RunT c m ()
c = echoCommand True

v :: MonadIO m => RunT c m ()
v = echoOnFailure False >> setPrefixes (Just (" 1> ", " 2> ")) >> echoOutput False

d :: MonadIO m => RunT c m ()
d = dotsPerChar 50 >> echoOutput False >> setPrefixes Nothing

f :: MonadIO m => RunT c m ()
f = exceptionOnFailure True

e :: MonadIO m => RunT c m ()
e = exceptionOnFailure True >> echoOutput False >> echoOnFailure True >> setPrefixes (Just (" 1> ", " 2> "))

-- | No output.
runProcessQ :: (P.NonBlocking c, MonadIO m) => (CreateProcess -> CreateProcess) -> CmdSpec -> c -> m [P.Output c]
runProcessQ modify cmd input = withRunState defaultRunState (runProcessM modify cmd input)

-- | Dot output
runProcessD :: (P.NonBlocking c, MonadIO m) => (CreateProcess -> CreateProcess) -> CmdSpec -> c -> m [P.Output c]
runProcessD modify cmd input =
    withRunState defaultRunState (c >> d >> runProcessM modify cmd input)

-- | Echo output
runProcessV :: (P.NonBlocking c, MonadIO m) => (CreateProcess -> CreateProcess) -> CmdSpec -> c -> m [P.Output c]
runProcessV modify cmd input =
    withRunState defaultRunState (c >> v >> runProcessM modify cmd input)

-- | Exception on failure
runProcessQF :: (P.NonBlocking c, MonadIO m) => (CreateProcess -> CreateProcess) -> CmdSpec -> c -> m [P.Output c]
runProcessQF modify cmd input =
    withRunState defaultRunState (c >> f >> runProcessM modify cmd input)

-- | Dot output and exception on failure
runProcessDF :: (P.NonBlocking c, MonadIO m) => (CreateProcess -> CreateProcess) -> CmdSpec -> c -> m [P.Output c]
runProcessDF modify cmd input =
    withRunState defaultRunState (c >> d >> f >> runProcessM modify cmd input)

-- | Echo output and exception on failure
runProcessVF :: (P.NonBlocking c, MonadIO m) => (CreateProcess -> CreateProcess) -> CmdSpec -> c -> m [P.Output c]
runProcessVF modify cmd input =
    withRunState defaultRunState (c >> v >> f >> runProcessM modify cmd input)

-- | Exception and echo on failure
runProcessQE :: (P.NonBlocking c, MonadIO m) => (CreateProcess -> CreateProcess) -> CmdSpec -> c -> m [P.Output c]
runProcessQE modify cmd input =
    withRunState defaultRunState (c >> e >> runProcessM modify cmd input)

-- | Dot output, exception on failure, echo on failure.  Note that
-- runProcessVE isn't a useful option, you get the output twice.  VF
-- makes more sense.
runProcessDE :: (P.NonBlocking c, MonadIO m) => (CreateProcess -> CreateProcess) -> CmdSpec -> c -> m [P.Output c]
runProcessDE modify cmd input =
    withRunState defaultRunState (c >> d >> e >> runProcessM modify cmd input)

showCommand :: CmdSpec -> String
showCommand (RawCommand cmd args) = showCommandForUser cmd args
showCommand (ShellCommand cmd) = cmd

-- | Select from the other runProcess* functions based on a verbosity level
runProcess :: (P.NonBlocking c, MonadIO m) => (CreateProcess -> CreateProcess) -> CmdSpec -> c -> m [P.Output c]
runProcess modify cmd input = liftIO $ 
    verbosity >>= \ v ->
    case v of
      _ | v <= 0 -> runProcessQ modify cmd input
      1 -> runProcessD modify cmd input
      _ -> runProcessV modify cmd input

-- | A version of 'runProcess' that throws an exception on failure.
runProcessF :: (P.NonBlocking c, MonadIO m) => (CreateProcess -> CreateProcess) -> CmdSpec -> c -> m [P.Output c]
runProcessF modify cmd input = liftIO $
    verbosity >>= \ v ->
    case v of
      _ | v <= 0 -> runProcessQF modify cmd input
      1 -> runProcessDE modify cmd input
      2 -> runProcessQE modify cmd input
      _ -> runProcessVF modify cmd input
