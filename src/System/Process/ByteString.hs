{-# OPTIONS_GHC -fno-warn-orphans #-}
module System.Process.ByteString where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import System.Exit (ExitCode)
import System.Process (CreateProcess, CmdSpec)
import qualified System.Process.Read as Read
import qualified System.Process.Read2 as Read2

instance Read.Strng ByteString where
  lazy _ = False
  length = fromInteger . toInteger . B.length
  null = B.null
  hPutStr = B.hPutStr
  hGetContents = B.hGetContents

instance Read2.Strng2 B.ByteString where
  hGetNonBlocking = B.hGetNonBlocking

-- | 'System.Process.Read.readProcessWithExitCode' specialized for 'ByteString'.
readProcessWithExitCode
    :: FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> ByteString               -- ^ standard input
    -> IO (ExitCode, ByteString, ByteString) -- ^ exitcode, stdout, stderr
readProcessWithExitCode = Read.readProcessWithExitCode

-- | 'System.Process.Read.readModifiedProcessWithExitCode' specialized for 'ByteString'.
readModifiedProcessWithExitCode
    :: (CreateProcess -> CreateProcess)
                                -- ^ Modify CreateProcess with this
    -> CmdSpec                  -- ^ command to run
    -> ByteString               -- ^ standard input
    -> IO (ExitCode, ByteString, ByteString, Maybe IOError) -- ^ exitcode, stdout, stderr, exception
readModifiedProcessWithExitCode = Read.readModifiedProcessWithExitCode

-- | 'System.Process.Read.readprocess' specialized for 'ByteString'.
readProcess
    :: FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> ByteString               -- ^ standard input
    -> IO ByteString            -- ^ stdout
readProcess = Read.readProcess

-- | 'System.Process.Read.readModifiedProcess' specialized for 'ByteString'.
readModifiedProcess
    :: (CreateProcess -> CreateProcess)
                                -- ^ Modify CreateProcess with this
    -> (IOError -> IO ())       -- ^ What to on ResourceVanished exception - usually throw or const (return ())
    -> CmdSpec                  -- ^ command to run
    -> ByteString               -- ^ standard input
    -> IO ByteString            -- ^ stdout
readModifiedProcess = Read.readModifiedProcess

-- | 'System.Process.Read2.readProcessChunksWithExitCode' specialized for 'ByteString'.
readProcessChunksWithExitCode
    :: (CreateProcess -> CreateProcess)
    -> CmdSpec                  -- ^ any arguments
    -> ByteString
    -> IO [Read2.Output ByteString]
readProcessChunksWithExitCode = Read2.readProcessChunksWithExitCode
