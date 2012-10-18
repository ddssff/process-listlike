{-# OPTIONS_GHC -fno-warn-orphans #-}
module System.Process.Read.ByteString.Lazy where

import qualified Data.ByteString.Lazy as L
import System.Exit (ExitCode)
import System.IO (hSetBinaryMode)
import System.Process (CreateProcess, CmdSpec)
import qualified System.Process.Read.Chars as Chars
import qualified System.Process.Read.Chunks as Chunks

instance Chars.Chars L.ByteString where
  init _ = mapM_ (\ h -> hSetBinaryMode h True) -- Prevent decoding errors when reading handles
  lazy _ = True
  length = L.length
  null = L.null
  hPutStr = L.hPutStr
  hGetContents = L.hGetContents

instance Chunks.NonBlocking L.ByteString where
  hGetNonBlocking = L.hGetNonBlocking

-- | 'System.Process.Read.readProcessWithExitCode' specialized for 'L.ByteString'.
readProcessWithExitCode
    :: FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> L.ByteString             -- ^ standard input
    -> IO (ExitCode, L.ByteString, L.ByteString) -- ^ exitcode, stdout, stderr
readProcessWithExitCode = Chars.readProcessWithExitCode

-- | 'System.Process.Read.readModifiedProcessWithExitCode' specialized for 'L.ByteString'.
readModifiedProcessWithExitCode
    :: (CreateProcess -> CreateProcess)
                                -- ^ Modify CreateProcess with this
    -> CmdSpec                  -- ^ command to run
    -> L.ByteString             -- ^ standard input
    -> IO (ExitCode, L.ByteString, L.ByteString) -- ^ exitcode, stdout, stderr, exception
readModifiedProcessWithExitCode = Chars.readModifiedProcessWithExitCode

-- | 'System.Process.Read.readprocess' specialized for 'L.ByteString'.
readProcess
    :: FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> L.ByteString             -- ^ standard input
    -> IO L.ByteString          -- ^ stdout
readProcess = Chars.readProcess

-- | 'System.Process.Read.readModifiedProcess' specialized for 'L.ByteString'.
readModifiedProcess
    :: (CreateProcess -> CreateProcess)
                                -- ^ Modify CreateProcess with this
    -> CmdSpec                  -- ^ command to run
    -> L.ByteString             -- ^ standard input
    -> IO L.ByteString          -- ^ stdout
readModifiedProcess = Chars.readModifiedProcess

-- | 'System.Process.Read2.readProcessChunks' specialized for 'L.ByteString'.
readProcessChunks
    :: (CreateProcess -> CreateProcess)
                                -- ^ Modify CreateProcess with this
    -> CmdSpec                  -- ^ command to run
    -> L.ByteString             -- ^ standard input
    -> IO [Chunks.Output L.ByteString]
readProcessChunks = Chunks.readProcessChunks
