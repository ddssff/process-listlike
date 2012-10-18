{-# OPTIONS_GHC -fno-warn-orphans #-}
module System.Process.Read.Text.Lazy where

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import System.Exit (ExitCode)
import System.Process (CreateProcess, CmdSpec)
import qualified System.Process.Read.Chars as Chars

instance Chars.Chars LT.Text where
  init _ _ = return ()
  lazy _ = True
  length = LT.length
  null = LT.null
  hPutStr = LT.hPutStr
  hGetContents = LT.hGetContents

-- | 'System.Process.Read.readProcessWithExitCode' specialized for 'LT.Text'.
readProcessWithExitCode
    :: FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> Text                     -- ^ standard input
    -> IO (ExitCode, Text, Text) -- ^ exitcode, stdout, stderr
readProcessWithExitCode = Chars.readProcessWithExitCode

-- | 'System.Process.Read.readModifiedProcessWithExitCode' specialized for 'LT.Text'.
readModifiedProcessWithExitCode
    :: (CreateProcess -> CreateProcess)
                                -- ^ Modify CreateProcess with this
    -> CmdSpec                  -- ^ command to run
    -> Text                     -- ^ standard input
    -> IO (ExitCode, Text, Text) -- ^ exitcode, stdout, stderr, exception
readModifiedProcessWithExitCode = Chars.readModifiedProcessWithExitCode

-- | 'System.Process.Read.readProcess' specialized for 'LT.Text'.
readProcess
    :: FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> Text                     -- ^ standard input
    -> IO Text                  -- ^ stdout
readProcess = Chars.readProcess

-- | 'System.Process.Read.readModifiedProcess' specialized for 'LT.Text'.
readModifiedProcess
    :: (CreateProcess -> CreateProcess)
                                -- ^ Modify CreateProcess with this
    -> CmdSpec                  -- ^ command to run
    -> Text                     -- ^ standard input
    -> IO Text                  -- ^ stdout
readModifiedProcess = Chars.readModifiedProcess
