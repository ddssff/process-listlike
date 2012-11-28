{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module System.Process.Text.Lazy
    ( readProcess
    , readProcessWithExitCode
    , readModifiedProcess
    , readModifiedProcessWithExitCode
    ) where

import System.Exit (ExitCode)
import System.Process (CreateProcess)
import qualified System.Process.Read as R

readProcess :: R.ListLikePlus a c => FilePath -> [String] -> a -> IO a
readProcess = R.readProcess
readProcessWithExitCode :: R.ListLikePlus a c => FilePath -> [String] -> a -> IO (ExitCode, a, a)
readProcessWithExitCode = R.readProcessWithExitCode
readModifiedProcess :: R.ListLikePlus a c => CreateProcess -> a -> IO a
readModifiedProcess = R.readModifiedProcess
readModifiedProcessWithExitCode :: R.ListLikePlus a c => CreateProcess -> a -> IO (ExitCode, a, a)
readModifiedProcessWithExitCode = R.readModifiedProcessWithExitCode
