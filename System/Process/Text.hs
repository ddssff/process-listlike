-- | "System.Process.ListLike" functions restricted to type
-- 'Data.Text.Text'.  gAll the module's supporting functions are also
-- re-exported here.
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module System.Process.Text
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

import Data.Text (Text)
import System.Exit (ExitCode)
import System.IO (Handle)
import System.Process (CreateProcess)
import System.Process.ListLike.Chunks hiding (readProcessChunks)
import System.Process.ListLike.Class hiding
    (readProcess, readProcessWithExitCode,
     readCreateProcess, readCreateProcessWithExitCode,
     readProcessInterleaved, readInterleaved, readProcessChunks)
import qualified System.Process.ListLike.Class as R
import System.Process.ListLike.Instances ()

readProcess :: (a ~ Text) => FilePath -> [String] -> a -> IO a
readProcess = R.readProcess
readProcessWithExitCode :: (a ~ Text) => FilePath -> [String] -> a -> IO (ExitCode, a, a)
readProcessWithExitCode = R.readProcessWithExitCode
readCreateProcess :: (a ~ Text) => CreateProcess -> a -> IO a
readCreateProcess = R.readCreateProcess
readCreateProcessWithExitCode :: (a ~ Text) => CreateProcess -> a -> IO (ExitCode, a, a)
readCreateProcessWithExitCode = R.readCreateProcessWithExitCode
readProcessInterleaved :: (a ~ Text, ProcessOutput a b) => CreateProcess -> a -> IO b
readProcessInterleaved = R.readProcessInterleaved
readInterleaved :: (a ~ Text, ProcessOutput a b) => [(a -> b, Handle)] -> IO b
readInterleaved = R.readInterleaved
readProcessChunks :: (a ~ Text) => CreateProcess -> a -> IO [R.Chunk a]
readProcessChunks = R.readProcessChunks
