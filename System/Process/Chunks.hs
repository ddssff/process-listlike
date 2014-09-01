module System.Process.Chunks
    ( mapOutput
    ) where

import System.Exit (ExitCode)
import System.Process (ProcessHandle)
import System.Process.ListLike (Output(..))

mapOutput :: (ProcessHandle -> b) -> (a -> b) -> (a -> b) -> (IOError -> b) -> (ExitCode -> b) -> Output a -> b
mapOutput pidf _ _ _ _ (ProcessHandle x) = pidf x
mapOutput _ outf _ _ _ (Stdout x) = outf x
mapOutput _ _ errf _ _ (Stderr x) = errf x
mapOutput _ _ _ exnf _ (Exception x) = exnf x
mapOutput _ _ _ _ exitf (Result x) = exitf x
