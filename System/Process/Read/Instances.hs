{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module System.Process.Read.Instances where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Int (Int64)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Word (Word8)
import System.IO (hSetBinaryMode)
import System.Process.Read.ListLike (ListLikePlus(..))

instance ListLikePlus String Char where
  type LengthType String = Int
  binary _ = mapM_ (\ h -> hSetBinaryMode h True) -- Prevent decoding errors when reading handles (because internally this uses lazy bytestrings)
  lazy _ = True
  length' = length

instance ListLikePlus B.ByteString Word8 where
  type LengthType B.ByteString = Int
  binary _ = mapM_ (\ h -> hSetBinaryMode h True) -- Prevent decoding errors when reading handles
  lazy _ = False
  length' = B.length

instance ListLikePlus L.ByteString Word8 where
  type LengthType L.ByteString = Int64
  binary _ = mapM_ (\ h -> hSetBinaryMode h True) -- Prevent decoding errors when reading handles
  lazy _ = True
  length' = L.length

instance ListLikePlus T.Text Char where
  type LengthType T.Text = Int
  binary _ _ = return ()
  lazy _ = False
  length' = T.length

instance ListLikePlus LT.Text Char where
  type LengthType LT.Text = Int64
  binary _ _ = return ()
  lazy _ = True
  length' = LT.length
