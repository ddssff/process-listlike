module System.Process.Read
    ( Chars(..)
    , readModifiedProcessWithExitCode
    , readModifiedProcess
    , readProcessWithExitCode
    , readProcess
    , Output(..)
    , NonBlocking(..)
    , foldOutput
    , foldOutputsL
    , foldOutputsR
    , readProcessChunks
    , readProcessChunks'
    ) where

import System.Process.Read.Chars
import System.Process.Read.Chunks
import System.Process.Read.Instances ()
