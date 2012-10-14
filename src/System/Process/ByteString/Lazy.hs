{-# OPTIONS_GHC -fno-warn-orphans #-}
module System.Process.ByteString.Lazy where

import qualified Data.ByteString.Lazy as L
import System.Exit (ExitCode)
import System.Process (CreateProcess)
import qualified System.Process.Read as Read

instance Read.Strng L.ByteString where
  null = L.null
  hPutStr = L.hPutStr
  hGetContents = L.hGetContents

readProcessWithExitCode
    :: FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> L.ByteString             -- ^ standard input
    -> IO (ExitCode, L.ByteString, L.ByteString) -- ^ exitcode, stdout, stderr
readProcessWithExitCode = Read.readProcessWithExitCode

readModifiedProcessWithExitCode
    :: (CreateProcess -> CreateProcess)
                                -- ^ Modify CreateProcess with this
    -> FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> L.ByteString             -- ^ standard input
    -> IO (ExitCode, L.ByteString, L.ByteString, Maybe IOError) -- ^ exitcode, stdout, stderr, exception
readModifiedProcessWithExitCode = Read.readModifiedProcessWithExitCode

readProcess
    :: FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> L.ByteString             -- ^ standard input
    -> IO L.ByteString          -- ^ stdout
readProcess = Read.readProcess

readModifiedProcess
    :: (CreateProcess -> CreateProcess)
                                -- ^ Modify CreateProcess with this
    -> (IOError -> IO ())       -- ^ What to on ResourceVanished exception - usually throw or const (return ())
    -> FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> L.ByteString             -- ^ standard input
    -> IO L.ByteString          -- ^ stdout
readModifiedProcess = Read.readModifiedProcess
