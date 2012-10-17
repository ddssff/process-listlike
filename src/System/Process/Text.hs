{-# OPTIONS_GHC -fno-warn-orphans #-}
module System.Process.Text where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Exit (ExitCode)
import System.Process (CreateProcess, CmdSpec)
import qualified System.Process.Read as Read

instance Read.Strng Text where
  init _ _ = return () 
  lazy _ = False
  length = fromInteger . toInteger . T.length
  null = T.null
  hPutStr = T.hPutStr
  hGetContents = T.hGetContents

-- | 'System.Process.Read.readProcessWithExitCode' specialized for 'T.Text'.
readProcessWithExitCode
    :: FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> Text                     -- ^ standard input
    -> IO (ExitCode, Text, Text) -- ^ exitcode, stdout, stderr
readProcessWithExitCode = Read.readProcessWithExitCode

-- | 'System.Process.Read.readModifiedProcessWithExitCode' specialized for 'T.Text'.
readModifiedProcessWithExitCode
    :: (CreateProcess -> CreateProcess)
                                -- ^ Modify CreateProcess with this
    -> CmdSpec                  -- ^ command to run
    -> Text                     -- ^ standard input
    -> IO (ExitCode, Text, Text, Maybe IOError) -- ^ exitcode, stdout, stderr, exception
readModifiedProcessWithExitCode = Read.readModifiedProcessWithExitCode

-- | 'System.Process.Read.readProcess' specialized for 'T.Text'.
readProcess
    :: FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> Text                     -- ^ standard input
    -> IO Text                  -- ^ stdout
readProcess = Read.readProcess

-- | 'System.Process.Read.readModifiedProcess' specialized for 'T.Text'.
readModifiedProcess
    :: (CreateProcess -> CreateProcess)
                                -- ^ Modify CreateProcess with this
    -> (IOError -> IO ())       -- ^ What to on ResourceVanished exception - usually throw or const (return ())
    -> CmdSpec                  -- ^ command to run
    -> Text                     -- ^ standard input
    -> IO Text                  -- ^ stdout
readModifiedProcess = Read.readModifiedProcess
