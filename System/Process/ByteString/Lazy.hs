-- | "System.Process.ListLike" functions restricted to the lazy 'Data.ByteString.Lazy.Char8.ByteString' type.
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module System.Process.ByteString.Lazy
    ( readProcess
    , readProcessWithExitCode
    , readCreateProcess
    , readCreateProcessWithExitCode
    , readProcessChunks
    ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import System.Exit (ExitCode)
import System.Process (CreateProcess)
import qualified System.Process.ListLike as R
import System.Process.Lazy ()

readProcess ::(a ~ ByteString) => FilePath -> [String] -> a -> IO a
readProcess = R.readProcess
readProcessWithExitCode ::(a ~ ByteString) => FilePath -> [String] -> a -> IO (ExitCode, a, a)
readProcessWithExitCode = R.readProcessWithExitCode
readCreateProcess ::(a ~ ByteString) => CreateProcess -> a -> IO a
readCreateProcess = R.readCreateProcess
readCreateProcessWithExitCode ::(a ~ ByteString) => CreateProcess -> a -> IO (ExitCode, a, a)
readCreateProcessWithExitCode = R.readCreateProcessWithExitCode
readProcessChunks :: (a ~ ByteString) => CreateProcess -> a -> IO [R.Chunk a]
readProcessChunks = R.readProcessChunks
