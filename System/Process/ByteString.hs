{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module System.Process.ByteString
    ( readProcess
    , readProcessWithExitCode
    , readCreateProcess
    , readCreateProcessWithExitCode
    ) where

import Data.ByteString.Char8 (ByteString)
import System.Exit (ExitCode)
import System.Process (CreateProcess)
import qualified System.Process.ListLike as R

readProcess :: (a ~ ByteString) => FilePath -> [String] -> a -> IO a
readProcess = R.readProcess
readProcessWithExitCode :: (a ~ ByteString) => FilePath -> [String] -> a -> IO (ExitCode, a, a)
readProcessWithExitCode = R.readProcessWithExitCode
readCreateProcess :: (a ~ ByteString) => CreateProcess -> a -> IO a
readCreateProcess = R.readCreateProcess
readCreateProcessWithExitCode :: (a ~ ByteString) => CreateProcess -> a -> IO (ExitCode, a, a)
readCreateProcessWithExitCode = R.readCreateProcessWithExitCode
