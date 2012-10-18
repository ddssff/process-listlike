{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module System.Process.Read.String where

import Control.Applicative ((<$>))
import Data.ByteString.UTF8 (toString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import System.Exit (ExitCode)
import System.IO (hPutStr, hGetContents, hSetBinaryMode)
import System.Process (CreateProcess, CmdSpec)
import System.Process.Read.Text ({- instance Read Text -})
import qualified System.Process.Read.Chars as Chars
import qualified System.Process.Read.Chunks as Chunks

instance Chars.Chars String where
  init _ = mapM_ (\ h -> hSetBinaryMode h True) -- Prevent decoding errors when reading handles (because internally this uses lazy bytestrings)
  lazy _ = False
  length = fromInteger . toInteger . length
  null = null
  hPutStr = hPutStr
  hGetContents = hGetContents

instance Chunks.NonBlocking String where
  hGetNonBlocking n h = (toString . B.concat . L.toChunks) <$> L.hGetNonBlocking n h

-- | 'System.Process.Read.readProcessWithExitCode' specialized for 'T.Text'.
readProcessWithExitCode
    :: FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> String                   -- ^ standard input
    -> IO (ExitCode, String, String) -- ^ exitcode, stdout, stderr
readProcessWithExitCode cmd args input =
    Chars.readProcessWithExitCode cmd args (T.pack input) >>= \ (code, out, err) ->
    return (code, T.unpack out, T.unpack err)

-- | 'System.Process.Read.readModifiedProcessWithExitCode' specialized for 'T.Text'.
readModifiedProcessWithExitCode
    :: (CreateProcess -> CreateProcess)
                                -- ^ Modify CreateProcess with this
    -> CmdSpec                  -- ^ command to run
    -> String                   -- ^ standard input
    -> IO (ExitCode, String, String) -- ^ exitcode, stdout, stderr, exception
readModifiedProcessWithExitCode modify cmd input =
    Chars.readModifiedProcessWithExitCode modify cmd (T.pack input) >>= \ (code, out, err) ->
    return (code, T.unpack out, T.unpack err)

-- | 'System.Process.Read.readProcess' specialized for 'T.Text'.
readProcess
    :: FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> String                   -- ^ standard input
    -> IO String                -- ^ stdout
readProcess cmd args input =
    T.unpack <$> Chars.readProcess cmd args (T.pack input)

-- | 'System.Process.Read.readModifiedProcess' specialized for 'T.Text'.
readModifiedProcess
    :: (CreateProcess -> CreateProcess)
                                -- ^ Modify CreateProcess with this
    -> CmdSpec                  -- ^ command to run
    -> String                   -- ^ standard input
    -> IO String                -- ^ stdout
readModifiedProcess modify cmd input =
    T.unpack <$> Chars.readModifiedProcess modify cmd (T.pack input)

-- | 'System.Process.Read2.readProcessChunks' specialized for 'ByteString'.
readProcessChunks
    :: (CreateProcess -> CreateProcess)
    -> CmdSpec                  -- ^ any arguments
    -> String
    -> IO [Chunks.Output String]
readProcessChunks = Chunks.readProcessChunks
