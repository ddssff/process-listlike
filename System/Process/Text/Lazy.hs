{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module System.Process.Text.Lazy
    ( readProcess
    , readProcessWithExitCode
    , readCreateProcess
    , readCreateProcessWithExitCode
    ) where

import System.Exit (ExitCode)
import System.Process (CreateProcess)
import qualified System.Process.Read as R

readProcess :: R.ListLikePlus a c => FilePath -> [String] -> a -> IO a
readProcess = R.readProcess
readProcessWithExitCode :: R.ListLikePlus a c => FilePath -> [String] -> a -> IO (ExitCode, a, a)
readProcessWithExitCode = R.readProcessWithExitCode
readCreateProcess :: R.ListLikePlus a c => CreateProcess -> a -> IO a
readCreateProcess = R.readCreateProcess
readCreateProcessWithExitCode :: R.ListLikePlus a c => CreateProcess -> a -> IO (ExitCode, a, a)
readCreateProcessWithExitCode = R.readCreateProcessWithExitCode
