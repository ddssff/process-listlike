-- | "System.Process.ListLike" functions restricted to type 'String'.
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module System.Process.String
    ( readProcess
    , readProcessWithExitCode
    , readCreateProcess
    , readCreateProcessWithExitCode
    , readProcessChunks
    ) where

import System.Exit (ExitCode)
import System.Process (CreateProcess)
import qualified System.Process.ListLike as R
import System.Process.Lazy ()

readProcess :: (a ~ String) => FilePath -> [String] -> a -> IO a
readProcess = R.readProcess
readProcessWithExitCode :: (a ~ String) => FilePath -> [String] -> a -> IO (ExitCode, a, a)
readProcessWithExitCode = R.readProcessWithExitCode
readCreateProcess :: (a ~ String) => CreateProcess -> a -> IO a
readCreateProcess = R.readCreateProcess
readCreateProcessWithExitCode :: (a ~ String) => CreateProcess -> a -> IO (ExitCode, a, a)
readCreateProcessWithExitCode = R.readCreateProcessWithExitCode
readProcessChunks :: (a ~ String) => CreateProcess -> a -> IO [R.Chunk a]
readProcessChunks = R.readProcessChunks
