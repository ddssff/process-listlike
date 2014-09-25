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
    , Chunks.Chunk(..)
    , readProcessChunks
    ) where

import System.Exit (ExitCode)
import System.IO (Handle)
import System.Process (CreateProcess)
import qualified System.Process.ListLike.Chunks as Chunks (Chunk(..), readProcessChunks)
import System.Process.ListLike.Class
import qualified System.Process.ListLike.Thread as R
import System.Process.ListLike.Instances ()

readProcess :: (a ~ String) => FilePath -> [String] -> a -> IO a
readProcess = R.readProcess
readProcessWithExitCode :: (a ~ String) => FilePath -> [String] -> a -> IO (ExitCode, a, a)
readProcessWithExitCode = R.readProcessWithExitCode
readCreateProcess :: (a ~ String) => CreateProcess -> a -> IO a
readCreateProcess = R.readCreateProcess
readCreateProcessWithExitCode :: (a ~ String) => CreateProcess -> a -> IO (ExitCode, a, a)
readCreateProcessWithExitCode = R.readCreateProcessWithExitCode
readProcessInterleaved :: (a ~ String, ProcessOutput a b) => CreateProcess -> a -> IO b
readProcessInterleaved = R.readProcessInterleaved
readInterleaved :: (a ~ String, ProcessOutput a b) => [(a -> b, Handle)] -> IO b
readInterleaved = R.readInterleaved
readProcessChunks :: (a ~ String) => CreateProcess -> a -> IO [Chunks.Chunk a]
readProcessChunks = Chunks.readProcessChunks
