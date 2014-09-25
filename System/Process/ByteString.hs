-- | "System.Process.ListLike" functions restricted to type 'Data.ByteString.Char8.ByteString'.
-- All the module's supporting functions are also re-exported here.
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module System.Process.ByteString
    ( readProcess
    , readProcessWithExitCode
    , readCreateProcess
    , readCreateProcessWithExitCode
    , readProcessInterleaved
    , readInterleaved
    , readProcessChunks
    , module System.Process.ListLike.Class
    , module System.Process.ListLike.Chunks
    ) where

import Data.ByteString.Char8 (ByteString)
import System.Exit (ExitCode)
import System.IO (Handle)
import System.Process (CreateProcess)
import System.Process.ListLike.Chunks hiding (readProcessChunks)
import System.Process.ListLike.Class (ProcessOutput)
import qualified System.Process.ListLike.Thread as R
import qualified System.Process.ListLike.Chunks as R (Chunk, readProcessChunks)
import System.Process.ListLike.Instances ()

readProcess :: (a ~ ByteString) => FilePath -> [String] -> a -> IO a
readProcess = R.readProcess
readProcessWithExitCode :: (a ~ ByteString) => FilePath -> [String] -> a -> IO (ExitCode, a, a)
readProcessWithExitCode = R.readProcessWithExitCode
readCreateProcess :: (a ~ ByteString) => CreateProcess -> a -> IO a
readCreateProcess = R.readCreateProcess
readCreateProcessWithExitCode :: (a ~ ByteString) => CreateProcess -> a -> IO (ExitCode, a, a)
readCreateProcessWithExitCode = R.readCreateProcessWithExitCode
readProcessInterleaved :: (a ~ ByteString, ProcessOutput a b) => CreateProcess -> a -> IO b
readProcessInterleaved = R.readProcessInterleaved
readInterleaved :: (a ~ ByteString, ProcessOutput a b) => [(a -> b, Handle)] -> IO b -> IO b
readInterleaved = R.readInterleaved
readProcessChunks :: (a ~ ByteString) => CreateProcess -> a -> IO [R.Chunk a]
readProcessChunks = R.readProcessChunks
