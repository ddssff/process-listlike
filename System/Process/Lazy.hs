{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module System.Process.Lazy where

import Control.Exception as E (evaluate)
import Control.Monad
import qualified Data.ByteString.Lazy as L
import Data.List as List (map, concat)
import Data.ListLike (ListLikeIO(..))
import Data.ListLike.Text.Text ()
import Data.ListLike.Text.TextLazy ()
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Word (Word8)
import Prelude hiding (null, length, rem)
import System.IO hiding (hPutStr, hGetContents)
import System.Process.ListLike (ListLikePlus(..))

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
