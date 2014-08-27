module System.Process.Read.Chunks
    ( Output(..)
    , readProcessChunks
    ) where

import System.Exit (ExitCode)
import System.Process (CreateProcess)
import System.Process.Read.Interleaved

data Output a = Stdout a | Stderr a | Result ExitCode | Exception IOError deriving (Eq, Show)

readProcessChunks :: (Chunked a c) => CreateProcess -> a -> IO [Output a]
readProcessChunks p input =
    readProcessInterleaved (\ x -> [Result x]) (\ x -> [Stdout x]) (\ x -> [Stderr x]) p input
