module System.Process.Read
    ( Chars(..)
    , NonBlocking(..)
    , readModifiedProcessWithExitCode
    , readModifiedProcess
    , readProcessWithExitCode
    , readProcess
    , readProcessChunks
    ) where

import System.Process.Read.Chars
import System.Process.Read.Chunks
-- Pull in all the Chars and NonBlocking instances.
import System.Process.Read.String ()
import System.Process.Read.ByteString ()
import System.Process.Read.ByteString.Lazy ()
import System.Process.Read.Text ()
import System.Process.Read.Text.Lazy ()
