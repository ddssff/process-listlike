{-# OPTIONS_GHC -fno-warn-orphans #-}
module System.Process.ByteString where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import System.Exit (ExitCode)
import System.Process (CreateProcess, CmdSpec)
import qualified System.Process.Read as Read
import qualified System.Process.Read2 as Read2

instance Read.Strng ByteString where
  null = B.null
  hPutStr = B.hPutStr
  hGetContents = B.hGetContents

instance Read2.Strng2 B.ByteString where
  hGetNonBlocking = B.hGetNonBlocking
  length = fromInteger . toInteger . B.length

readProcessWithExitCode
    :: FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> ByteString               -- ^ standard input
    -> IO (ExitCode, ByteString, ByteString) -- ^ exitcode, stdout, stderr
readProcessWithExitCode = Read.readProcessWithExitCode

readModifiedProcessWithExitCode
    :: (CreateProcess -> CreateProcess)
                                -- ^ Modify CreateProcess with this
    -> CmdSpec                  -- ^ command to run
    -> ByteString               -- ^ standard input
    -> IO (ExitCode, ByteString, ByteString, Maybe IOError) -- ^ exitcode, stdout, stderr, exception
readModifiedProcessWithExitCode = Read.readModifiedProcessWithExitCode

readProcess
    :: FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> ByteString               -- ^ standard input
    -> IO ByteString            -- ^ stdout
readProcess = Read.readProcess

readModifiedProcess
    :: (CreateProcess -> CreateProcess)
                                -- ^ Modify CreateProcess with this
    -> (IOError -> IO ())       -- ^ What to on ResourceVanished exception - usually throw or const (return ())
    -> CmdSpec                  -- ^ command to run
    -> ByteString               -- ^ standard input
    -> IO ByteString            -- ^ stdout
readModifiedProcess = Read.readModifiedProcess

readProcessChunksWithExitCode
    :: (CreateProcess -> CreateProcess)
    -> CmdSpec                  -- ^ any arguments
    -> ByteString
    -> IO (ExitCode, [Read2.Output ByteString])
readProcessChunksWithExitCode = Read2.readProcessChunksWithExitCode
