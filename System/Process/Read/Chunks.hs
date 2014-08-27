module System.Process.Read.Chunks
    ( Output(..)
    , readProcessChunks
    ) where

import Control.DeepSeq (NFData)
import System.Exit (ExitCode)
import System.Process (CreateProcess)
import System.Process.Read.Interleaved

-- | This lets us use deepseq's force on the stream of data returned
-- by readProcessChunks.
instance NFData ExitCode

data Output a = Stdout a | Stderr a | Result ExitCode | Exception IOError deriving (Eq, Show)

readProcessChunks :: (Chunked a c) => CreateProcess -> a -> IO [Output a]
readProcessChunks p input =
    readProcessInterleaved (\ x -> [Result x]) (\ x -> [Stdout x]) (\ x -> [Stderr x]) p input
