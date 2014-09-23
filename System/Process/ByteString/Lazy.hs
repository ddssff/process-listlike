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
    , module System.Process.ListLike.Class
    , module System.Process.ListLike.Chunks
    ) where

import Control.Exception (AsyncException)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Monoid (Monoid)
import System.Exit (ExitCode)
import System.IO (Handle)
import System.Process (CreateProcess, ProcessHandle)
import System.Process.ListLike.Chunks hiding (readProcessChunks)
import System.Process.ListLike.Class hiding
    (readProcess, readProcessWithExitCode,
     readCreateProcess, readCreateProcessWithExitCode,
     readProcessInterleaved, readInterleaved, readProcessChunks)
import qualified System.Process.ListLike.Class as R
import System.Process.ListLike.Instances ()

readProcess ::(a ~ ByteString) => FilePath -> [String] -> a -> IO a
readProcess = R.readProcess
readProcessWithExitCode ::(a ~ ByteString) => FilePath -> [String] -> a -> IO (ExitCode, a, a)
readProcessWithExitCode = R.readProcessWithExitCode
readCreateProcess ::(a ~ ByteString) => CreateProcess -> a -> IO a
readCreateProcess = R.readCreateProcess
readCreateProcessWithExitCode ::(a ~ ByteString) => CreateProcess -> a -> IO (ExitCode, a, a)
readCreateProcessWithExitCode = R.readCreateProcessWithExitCode
readProcessInterleaved :: (a ~ ByteString, Monoid b) => (ProcessHandle -> b) -> (ExitCode -> b) -> (a -> b) -> (a -> b) -> (Either AsyncException IOError -> b) -> CreateProcess -> a -> IO b
readProcessInterleaved = R.readProcessInterleaved
readInterleaved :: (a ~ ByteString, Monoid b) => [(a -> b, Handle)] -> (Either AsyncException IOError -> b) -> IO b
readInterleaved = R.readInterleaved
readProcessChunks :: (a ~ ByteString) => CreateProcess -> a -> IO [R.Chunk a]
readProcessChunks = R.readProcessChunks
