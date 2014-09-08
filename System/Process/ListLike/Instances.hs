{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies, TypeSynonymInstances #-}
-- | ListLikePlus instances for strict types - these are more
-- dangerous, if you start a long running process with them they will
-- block until the process finishes.  Why not try a lazy type?
{-# OPTIONS_GHC -fno-warn-orphans #-}
module System.Process.ListLike.Instances where

import Control.DeepSeq (force)
import Control.Exception as E (evaluate)
import Data.ByteString.Char8 as B (ByteString)
import qualified Data.ByteString.Lazy as L
import Data.List as List (map, concat)
import Data.ListLike.IO (hGetContents)
import Data.Text as T (Text, unpack)
import qualified Data.Text.Lazy as LT
import Data.Word (Word8)
import System.IO (hSetBinaryMode)
import System.Process.ListLike.Class (ListLikePlus(..))

instance ListLikePlus B.ByteString Word8 where
  setModes _ (inh, outh, errh, _) = f inh >> f outh >> f errh where f mh = maybe (return ()) (\ h -> hSetBinaryMode h True) mh
  readChunks h = hGetContents h >>= return . force . (: [])

instance ListLikePlus T.Text Char where
  setModes _ _  = return ()
  readChunks h = hGetContents h >>= return . force . (: [])

instance ListLikePlus L.ByteString Word8 where
  setModes _ (inh, outh, errh, _) = f inh >> f outh >> f errh where f mh = maybe (return ()) (\ h -> hSetBinaryMode h True) mh
  readChunks h = hGetContents h >>= evaluate . Prelude.map (L.fromChunks . (: [])) . L.toChunks

instance ListLikePlus LT.Text Char where
  setModes _ _  = return ()
  readChunks h = hGetContents h >>= evaluate . Prelude.map (LT.fromChunks . (: [])) . LT.toChunks

-- | This String instance is implemented using the Lazy Text instance.
-- Otherwise (without some serious coding) String would be a strict
-- instance .  Note that the 'System.Process.readProcess' in the
-- process library is strict, while our equivalent is not - see test4
-- in Tests/Dots.hs.
instance ListLikePlus String Char where
  setModes _ _  = return ()
  readChunks h = readChunks h >>= return . List.map T.unpack . List.concat . List.map LT.toChunks
