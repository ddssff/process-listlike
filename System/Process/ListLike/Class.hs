{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}
module System.Process.ListLike.Class
    ( ListLikePlus(..)
    , ProcessOutput(..)
    , showCmdSpecForUser
    ) where

import Control.Exception (AsyncException)
import Data.ListLike (ListLikeIO(..))
import Data.ListLike.Text.Text ()
import Data.ListLike.Text.TextLazy ()
import Data.Monoid (Monoid(mempty, mappend))
import Prelude hiding (null, length, rem)
import System.Exit (ExitCode(ExitFailure))
import System.IO hiding (hPutStr, hGetContents)
import System.Process (ProcessHandle, CmdSpec(ShellCommand, RawCommand), showCommandForUser)

-- | Methods for turning the output of a process into a monoid.
class Monoid b => ProcessOutput a b | b -> a where
    pidf :: ProcessHandle -> b
    outf :: a -> b
    errf :: a -> b
    intf :: Either AsyncException IOError -> b
    codef :: ExitCode -> b

-- | A process should have one 'ExitCode' at the end, this monoid lets
-- us build a monoid for the type returned by readProcessWithExitCode.
instance Monoid ExitCode where
    mempty = ExitFailure 0
    mappend x (ExitFailure 0) = x
    mappend _ x = x

-- | Class of types which can be used as the input and outputs of
-- these process functions.
class ListLikeIO a c => ListLikePlus a c where
  setModes :: a -> (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO ()
  -- ^ Perform initialization on the handles returned by createProcess
  -- based on this ListLikePlus instance - typically setting binary
  -- mode on all the file descriptors if the element type is Word8.
  -- If this is not done, reading something other than text (such as a
  -- .jpg or .pdf file) will usually fail with a decoding error.
  readChunks :: Handle -> IO [a]
  -- ^ Read the list of chunks from this handle.  For lazy types this
  -- is just a call to hGetContents followed by toChunks.  For strict
  -- types it might return a singleton list.  Strings are trickier.

showCmdSpecForUser :: CmdSpec -> String
showCmdSpecForUser (ShellCommand s) = s
showCmdSpecForUser (RawCommand p args) = showCommandForUser p args
