module System.Process.Read
    ( ListLikePlus(..)
    , readCreateProcessWithExitCode
    , readCreateProcess
    , readProcessWithExitCode
    , readProcess
    , readProcessInterleaved
    , readInterleaved
    , Output(..)
    , readProcessChunks
    ) where

import System.Process.Read.Chunks
import System.Process.Read.ListLike
