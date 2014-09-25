-- | "System.Process.ListLike" functions restricted to the lazy
-- 'Data.ByteString.Lazy.Char8.ByteString' type.  All the module's
-- supporting functions are also re-exported here.
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module System.Process.ByteString.Lazy
    ( readProcess
    , readProcessWithExitCode
    , readCreateProcess
    , readCreateProcessWithExitCode
    , readProcessInterleaved
    , readInterleaved
    , Chunks.Chunk(..)
    , readProcessChunks
    ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import System.Exit (ExitCode)
import System.IO (Handle)
import System.Process (CreateProcess)
import qualified System.Process.ListLike.Chunks as Chunks (Chunk(..), readProcessChunks)
import System.Process.ListLike.Class
import qualified System.Process.ListLike.Thread as R
import System.Process.ListLike.Instances ()

readProcess ::(a ~ ByteString) => FilePath -> [String] -> a -> IO a
readProcess = R.readProcess
readProcessWithExitCode ::(a ~ ByteString) => FilePath -> [String] -> a -> IO (ExitCode, a, a)
readProcessWithExitCode = R.readProcessWithExitCode
readCreateProcess ::(a ~ ByteString) => CreateProcess -> a -> IO a
readCreateProcess = R.readCreateProcess
readCreateProcessWithExitCode ::(a ~ ByteString) => CreateProcess -> a -> IO (ExitCode, a, a)
readCreateProcessWithExitCode = R.readCreateProcessWithExitCode
readProcessInterleaved :: (a ~ ByteString, ProcessOutput a b) => CreateProcess -> a -> IO b
readProcessInterleaved = R.readProcessInterleaved
readInterleaved :: (a ~ ByteString, ProcessOutput a b) => [(a -> b, Handle)] -> IO b -> IO b
readInterleaved = R.readInterleaved
readProcessChunks :: (a ~ ByteString) => CreateProcess -> a -> IO [Chunks.Chunk a]
readProcessChunks = Chunks.readProcessChunks
