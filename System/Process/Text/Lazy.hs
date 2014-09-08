-- | "System.Process.ListLike" functions restricted to the lazy 'Data.Text.Lazy.Text' type.
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module System.Process.Text.Lazy
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

import Data.Text.Lazy (Text)
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

readProcess :: (a ~ Text) => FilePath -> [String] -> a -> IO a
readProcess = R.readProcess
readProcessWithExitCode :: (a ~ Text) => FilePath -> [String] -> a -> IO (ExitCode, a, a)
readProcessWithExitCode = R.readProcessWithExitCode
readCreateProcess :: (a ~ Text) => CreateProcess -> a -> IO a
readCreateProcess = R.readCreateProcess
readCreateProcessWithExitCode :: (a ~ Text) => CreateProcess -> a -> IO (ExitCode, a, a)
readCreateProcessWithExitCode = R.readCreateProcessWithExitCode
readProcessInterleaved :: (a ~ Text, Monoid b) => (ProcessHandle -> b) -> (ExitCode -> b) -> (a -> b) -> (a -> b) -> CreateProcess -> a -> IO b
readProcessInterleaved = R.readProcessInterleaved
readInterleaved :: (a ~ Text, Monoid b) => b -> [(a -> b, Handle)] -> IO b -> IO b
readInterleaved = R.readInterleaved
readProcessChunks :: (a ~ Text) => CreateProcess -> a -> IO [R.Chunk a]
readProcessChunks = R.readProcessChunks
