module System.Process.Read
    ( Chars(..)
    , readModifiedProcessWithExitCode
    , readModifiedProcess
    , readProcessWithExitCode
    , readProcess
    , NonBlocking(..)
    , Output(..)
    , readProcessChunks
    , foldOutput
    -- * Convenience functions
    , module System.Process.Read.Convenience
    -- * Compatibility functions
    , module System.Process.Read.Compat
    ) where

import System.Process.Read.Chars
import System.Process.Read.Chunks
import System.Process.Read.Instances ()
import System.Process.Read.Convenience
import System.Process.Read.Compat
