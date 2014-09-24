-- | Re-exporting module for the polymorphic portions of the
-- process-listlike package and the ListLikePlus instances.
module System.Process.ListLike (
  module System.Process.ListLike.Class,
  module System.Process.ListLike.Chunks,
  System.Process.proc,
  System.Process.shell
  ) where

import System.Process (proc, shell)
import System.Process.ListLike.Class
import System.Process.ListLike.Instances ()
import System.Process.ListLike.Chunks
