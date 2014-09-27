-- | "System.Process.ListLike" functions restricted to type 'String'.
-- All the module's supporting functions are also re-exported here.
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module System.Process.String
    ( readProcess
    , readProcessWithExitCode
    , readCreateProcess
    , readCreateProcessWithExitCode
    , readProcessInterleaved
    , readInterleaved
    , readProcessChunks
    , module System.Process
    , module System.Process.ListLike
    ) where

import System.Exit (ExitCode)
import System.IO (Handle)
import System.Process hiding (readProcess, readProcessWithExitCode)
import System.Process.ListLike hiding (readCreateProcess, readCreateProcessWithExitCode,
                                       readProcess, readProcessWithExitCode,
                                       readProcessInterleaved, readInterleaved, readProcessChunks)
import qualified System.Process.ListLike as LL

readProcess :: (a ~ String) => FilePath -> [String] -> a -> IO a
readProcess = LL.readProcess
readProcessWithExitCode :: (a ~ String) => FilePath -> [String] -> a -> IO (ExitCode, a, a)
readProcessWithExitCode = LL.readProcessWithExitCode
readCreateProcess :: (a ~ String) => CreateProcess -> a -> IO a
readCreateProcess = LL.readCreateProcess
readCreateProcessWithExitCode :: (a ~ String) => CreateProcess -> a -> IO (ExitCode, a, a)
readCreateProcessWithExitCode = LL.readCreateProcessWithExitCode
readProcessInterleaved :: (a ~ String, ProcessOutput a b) => CreateProcess -> a -> IO b
readProcessInterleaved = LL.readProcessInterleaved
readInterleaved :: (a ~ String, ProcessOutput a b) => [(a -> b, Handle)] -> IO b -> IO b
readInterleaved = LL.readInterleaved
readProcessChunks :: (a ~ String) => CreateProcess -> a -> IO [Chunk a]
readProcessChunks = LL.readProcessChunks
