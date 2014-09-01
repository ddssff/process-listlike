{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies, TypeSynonymInstances #-}
-- | ListLikePlus instances for strict types - these are more
-- dangerous, if you start a long running process with them they will
-- block until the process finishes.  Why not try a lazy type?
module System.Process.Strict where

import Data.ByteString.Char8 as B (ByteString, length)
import Data.Text as T (Text, length)
import Data.Word (Word8)
import System.IO (hSetBinaryMode)
import System.Process.ListLike (ListLikePlus(..))

{-
instance ListLikePlus String Char where
  type LengthType String = Int
  setModes _ _  = return ()
  lazy _ = True  -- Not quite sure why this is True.
  toChunks = (: [])

instance ListLikePlus B.ByteString Word8 where
  type LengthType B.ByteString = Int
  setModes _ (inh, outh, errh, _) = f inh >> f outh >> f errh where f mh = maybe (return ()) (\ h -> hSetBinaryMode h True) mh
  lazy _ = False
  toChunks = (: [])

instance ListLikePlus T.Text Char where
  type LengthType T.Text = Int
  setModes _ _  = return ()
  lazy _ = False
  toChunks = (: [])
-}
