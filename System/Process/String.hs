-- | "System.Process.ListLike" functions restricted to type 'String'.
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
    , module System.Process.ListLike
    , module System.Process.Chunks
    ) where

import Data.Monoid (Monoid)
import System.Exit (ExitCode)
import System.IO (Handle)
import System.Process (CreateProcess, ProcessHandle)
import System.Process.Chunks hiding (readProcessChunks)
import System.Process.ListLike hiding (readProcess, readProcessWithExitCode,
                                       readCreateProcess, readCreateProcessWithExitCode,
                                       readProcessInterleaved, readInterleaved, readProcessChunks)
import qualified System.Process.ListLike as R
import System.Process.ListLike.Instances ()

readProcess :: (a ~ String) => FilePath -> [String] -> a -> IO a
readProcess = R.readProcess
readProcessWithExitCode :: (a ~ String) => FilePath -> [String] -> a -> IO (ExitCode, a, a)
readProcessWithExitCode = R.readProcessWithExitCode
readCreateProcess :: (a ~ String) => CreateProcess -> a -> IO a
readCreateProcess = R.readCreateProcess
readCreateProcessWithExitCode :: (a ~ String) => CreateProcess -> a -> IO (ExitCode, a, a)
readCreateProcessWithExitCode = R.readCreateProcessWithExitCode
readProcessInterleaved :: (a ~ String, Monoid b) => (ProcessHandle -> b) -> (ExitCode -> b) -> (a -> b) -> (a -> b) -> CreateProcess -> a -> IO b
readProcessInterleaved = R.readProcessInterleaved
readInterleaved :: (a ~ String, Monoid b) => b -> [(a -> b, Handle)] -> IO b -> IO b
readInterleaved = R.readInterleaved
readProcessChunks :: (a ~ String) => CreateProcess -> a -> IO [R.Chunk a]
readProcessChunks = R.readProcessChunks
