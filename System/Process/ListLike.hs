-- | Re-exporting module for the polymorphic portions of the
-- process-listlike package and the ListLikePlus instances.
module System.Process.ListLike (
  module System.Process.ListLike.Class,
  module System.Process.ListLike.Chunks,
  module System.Process.ListLike.Ready,
  module System.Process.ListLike.Thread,
  module System.Process
  ) where

import System.Process hiding (readProcess, readProcessWithExitCode)
import System.Process.ListLike.Class
import System.Process.ListLike.Instances ()
import System.Process.ListLike.Chunks
import System.Process.ListLike.Ready hiding (readProcessChunks, readProcessInterleaved, readCreateProcessWithExitCode)
import System.Process.ListLike.Thread
