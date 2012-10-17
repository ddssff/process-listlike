{-# OPTIONS_GHC -fno-warn-orphans #-}
module System.Process.ByteString.Lazy where

import qualified Data.ByteString.Lazy as L
import System.Exit (ExitCode)
import System.IO (hSetBinaryMode)
import System.Process (CreateProcess, CmdSpec)
import qualified System.Process.Read as Read
import qualified System.Process.Read2 as Read2

instance Read.Strng L.ByteString where
  init _ = mapM_ (\ h -> hSetBinaryMode h True) -- Prevent decoding errors when reading handles
  lazy _ = True
  length = L.length
  null = L.null
  hPutStr = L.hPutStr
  hGetContents = L.hGetContents

instance Read2.Strng2 L.ByteString where
  hGetNonBlocking = L.hGetNonBlocking

-- | 'System.Process.Read.readProcessWithExitCode' specialized for 'L.ByteString'.
readProcessWithExitCode
    :: FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> L.ByteString             -- ^ standard input
    -> IO (ExitCode, L.ByteString, L.ByteString) -- ^ exitcode, stdout, stderr
readProcessWithExitCode = Read.readProcessWithExitCode

-- | 'System.Process.Read.readModifiedProcessWithExitCode' specialized for 'L.ByteString'.
readModifiedProcessWithExitCode
    :: (CreateProcess -> CreateProcess)
                                -- ^ Modify CreateProcess with this
    -> CmdSpec                  -- ^ command to run
    -> L.ByteString             -- ^ standard input
    -> IO (ExitCode, L.ByteString, L.ByteString, Maybe IOError) -- ^ exitcode, stdout, stderr, exception
readModifiedProcessWithExitCode = Read.readModifiedProcessWithExitCode

-- | 'System.Process.Read.readprocess' specialized for 'L.ByteString'.
readProcess
    :: FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> L.ByteString             -- ^ standard input
    -> IO L.ByteString          -- ^ stdout
readProcess = Read.readProcess

-- | 'System.Process.Read.readModifiedProcess' specialized for 'L.ByteString'.
readModifiedProcess
    :: (CreateProcess -> CreateProcess)
                                -- ^ Modify CreateProcess with this
    -> (IOError -> IO ())       -- ^ What to on ResourceVanished exception - usually throw or const (return ())
    -> CmdSpec                  -- ^ command to run
    -> L.ByteString             -- ^ standard input
    -> IO L.ByteString          -- ^ stdout
readModifiedProcess = Read.readModifiedProcess

-- | 'System.Process.Read2.readProcessChunksWithExitCode' specialized for 'L.ByteString'.
readProcessChunksWithExitCode
    :: (CreateProcess -> CreateProcess)
                                -- ^ Modify CreateProcess with this
    -> CmdSpec                  -- ^ command to run
    -> L.ByteString             -- ^ standard input
    -> IO [Read2.Output L.ByteString]
readProcessChunksWithExitCode = Read2.readProcessChunksWithExitCode
