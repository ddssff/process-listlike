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
    , Chunks.Chunk(..)
    , readProcessChunks
    ) where

import Data.Text (Text)
import System.Exit (ExitCode)
import System.IO (Handle)
import System.Process (CreateProcess, ProcessHandle)
import qualified System.Process.ListLike.Chunks as Chunks (Chunk(..), readProcessChunks)
import System.Process.ListLike.Class
import qualified System.Process.ListLike.Thread as R
import System.Process.ListLike.Instances ()

readProcess :: (a ~ Text) => FilePath -> [String] -> a -> IO a
readProcess = R.readProcess
readProcessWithExitCode :: (a ~ Text) => FilePath -> [String] -> a -> IO (ExitCode, a, a)
readProcessWithExitCode = R.readProcessWithExitCode
readCreateProcess :: (a ~ Text) => CreateProcess -> a -> IO a
readCreateProcess = R.readCreateProcess
readCreateProcessWithExitCode :: (a ~ Text) => CreateProcess -> a -> IO (ExitCode, a, a)
readCreateProcessWithExitCode = R.readCreateProcessWithExitCode
readProcessInterleaved :: (a ~ Text, ProcessOutput a b) => (ProcessHandle -> IO ()) -> CreateProcess -> a -> IO b
readProcessInterleaved = R.readProcessInterleaved
readInterleaved :: (a ~ Text, ProcessOutput a b) => [(a -> b, Handle)] -> IO b -> IO b
readInterleaved = R.readInterleaved
readProcessChunks :: (a ~ Text) => CreateProcess -> a -> IO [Chunks.Chunk a]
readProcessChunks = Chunks.readProcessChunks
