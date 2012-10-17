{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module System.Process.String where

import Control.Applicative ((<$>))
import Data.ByteString.UTF8 (toString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import System.Exit (ExitCode)
import System.IO (hPutStr, hGetContents)
import System.Process (CreateProcess, CmdSpec)
import System.Process.Text ({- instance Read Text -})
import qualified System.Process.Read as Read
import qualified System.Process.Read2 as Read2

instance Read.Strng String where
  lazy _ = False
  length = fromInteger . toInteger . length
  null = null
  hPutStr = hPutStr
  hGetContents = hGetContents

instance Read2.Strng2 String where
  hGetNonBlocking n h = (toString . B.concat . L.toChunks) <$> L.hGetNonBlocking n h

-- | 'System.Process.Read.readProcessWithExitCode' specialized for 'T.Text'.
readProcessWithExitCode
    :: FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> String                   -- ^ standard input
    -> IO (ExitCode, String, String) -- ^ exitcode, stdout, stderr
readProcessWithExitCode cmd args input =
    Read.readProcessWithExitCode cmd args (T.pack input) >>= \ (code, out, err) ->
    return (code, T.unpack out, T.unpack err)

-- | 'System.Process.Read.readModifiedProcessWithExitCode' specialized for 'T.Text'.
readModifiedProcessWithExitCode
    :: (CreateProcess -> CreateProcess)
                                -- ^ Modify CreateProcess with this
    -> CmdSpec                  -- ^ command to run
    -> String                   -- ^ standard input
    -> IO (ExitCode, String, String, Maybe IOError) -- ^ exitcode, stdout, stderr, exception
readModifiedProcessWithExitCode modify cmd input =
    Read.readModifiedProcessWithExitCode modify cmd (T.pack input) >>= \ (code, out, err, exn) ->
    return (code, T.unpack out, T.unpack err, exn)

-- | 'System.Process.Read.readProcess' specialized for 'T.Text'.
readProcess
    :: FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> String                   -- ^ standard input
    -> IO String                -- ^ stdout
readProcess cmd args input =
    T.unpack <$> Read.readProcess cmd args (T.pack input)

-- | 'System.Process.Read.readModifiedProcess' specialized for 'T.Text'.
readModifiedProcess
    :: (CreateProcess -> CreateProcess)
                                -- ^ Modify CreateProcess with this
    -> (IOError -> IO ())       -- ^ What to on ResourceVanished exception - usually throw or const (return ())
    -> CmdSpec                  -- ^ command to run
    -> String                   -- ^ standard input
    -> IO String                -- ^ stdout
readModifiedProcess modify handle cmd input =
    T.unpack <$> Read.readModifiedProcess modify handle cmd (T.pack input)
