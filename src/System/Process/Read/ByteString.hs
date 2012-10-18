{-# OPTIONS_GHC -fno-warn-orphans #-}
module System.Process.Read.ByteString where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import System.Exit (ExitCode)
import System.IO (hSetBinaryMode)
import System.Process (CreateProcess, CmdSpec)
import qualified System.Process.Read.Chars as Chars
import qualified System.Process.Read.Chunks as Chunks

instance Chars.Chars B.ByteString where
  init _ = mapM_ (\ h -> hSetBinaryMode h True) -- Prevent decoding errors when reading handles
  lazy _ = False
  length = fromInteger . toInteger . B.length
  null = B.null
  hPutStr = B.hPutStr
  hGetContents = B.hGetContents

instance Chunks.NonBlocking B.ByteString where
  hGetNonBlocking = B.hGetNonBlocking

-- | 'System.Process.Read.readProcessWithExitCode' specialized for 'ByteString'.
readProcessWithExitCode
    :: FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> ByteString               -- ^ standard input
    -> IO (ExitCode, ByteString, ByteString) -- ^ exitcode, stdout, stderr
readProcessWithExitCode = Chars.readProcessWithExitCode

-- | 'System.Process.Read.readModifiedProcessWithExitCode' specialized for 'ByteString'.
readModifiedProcessWithExitCode
    :: (CreateProcess -> CreateProcess)
                                -- ^ Modify CreateProcess with this
    -> CmdSpec                  -- ^ command to run
    -> ByteString               -- ^ standard input
    -> IO (ExitCode, ByteString, ByteString) -- ^ exitcode, stdout, stderr, exception
readModifiedProcessWithExitCode = Chars.readModifiedProcessWithExitCode

-- | 'System.Process.Read.readprocess' specialized for 'ByteString'.
readProcess
    :: FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> ByteString               -- ^ standard input
    -> IO ByteString            -- ^ stdout
readProcess = Chars.readProcess

-- | 'System.Process.Read.readModifiedProcess' specialized for 'ByteString'.
readModifiedProcess
    :: (CreateProcess -> CreateProcess)
                                -- ^ Modify CreateProcess with this
    -> CmdSpec                  -- ^ command to run
    -> ByteString               -- ^ standard input
    -> IO ByteString            -- ^ stdout
readModifiedProcess = Chars.readModifiedProcess

-- | 'System.Process.Read2.readProcessChunks' specialized for 'ByteString'.
readProcessChunks
    :: (CreateProcess -> CreateProcess)
    -> CmdSpec                  -- ^ any arguments
    -> ByteString
    -> IO [Chunks.Output ByteString]
readProcessChunks = Chunks.readProcessChunks
