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
    , readProcessChunks
    , module System.Process
    , module System.Process.ListLike
    ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import System.Exit (ExitCode)
import System.IO (Handle)
import System.Process hiding (readProcess, readProcessWithExitCode)
import System.Process.ListLike hiding (readCreateProcess, readCreateProcessWithExitCode,
                                       readProcess, readProcessWithExitCode,
                                       readProcessInterleaved, readInterleaved, readProcessChunks)
import qualified System.Process.ListLike as LL

readProcess ::(a ~ ByteString) => FilePath -> [String] -> a -> IO a
readProcess = LL.readProcess
readProcessWithExitCode ::(a ~ ByteString) => FilePath -> [String] -> a -> IO (ExitCode, a, a)
readProcessWithExitCode = LL.readProcessWithExitCode
readCreateProcess ::(a ~ ByteString) => CreateProcess -> a -> IO a
readCreateProcess = LL.readCreateProcess
readCreateProcessWithExitCode ::(a ~ ByteString) => CreateProcess -> a -> IO (ExitCode, a, a)
readCreateProcessWithExitCode = LL.readCreateProcessWithExitCode
readProcessInterleaved :: (a ~ ByteString, ProcessOutput a b) => (ProcessHandle -> IO ()) -> CreateProcess -> a -> IO b
readProcessInterleaved = LL.readProcessInterleaved
readInterleaved :: (a ~ ByteString, ProcessOutput a b) => [(a -> b, Handle)] -> IO b -> IO b
readInterleaved = LL.readInterleaved
readProcessChunks :: (a ~ ByteString) => CreateProcess -> a -> IO [Chunk a]
readProcessChunks = LL.readProcessChunks
