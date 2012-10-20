{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, TypeFamilies, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module System.Process.Read.Instances where

import Control.Applicative ((<$>))
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.ByteString.UTF8 (toString, fromString)
import Data.Int (Int64)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import Prelude hiding (catch)
import System.IO (hSetBinaryMode)
import qualified System.Process.Read.Chars as Chars
import qualified System.Process.Read.Chunks as Chunks

instance Chars.Chars String where
  type LengthType String = Int
  init _ = mapM_ (\ h -> hSetBinaryMode h True) -- Prevent decoding errors when reading handles (because internally this uses lazy bytestrings)
  lazy _ = True
  length = length
  null = null
  hPutStr h s = L.hPutStr h (L.fromChunks [fromString s])
  hGetContents h = (toString . B.concat . L.toChunks) <$> L.hGetContents h

instance Chunks.NonBlocking String where
  hGetNonBlocking n h = (toString . B.concat . L.toChunks) <$> L.hGetNonBlocking n h

instance Chars.Chars B.ByteString where
  type LengthType B.ByteString = Int
  init _ = mapM_ (\ h -> hSetBinaryMode h True) -- Prevent decoding errors when reading handles
  lazy _ = False
  length = B.length
  null = B.null
  hPutStr = B.hPutStr
  hGetContents = B.hGetContents

instance Chunks.NonBlocking B.ByteString where
  hGetNonBlocking = B.hGetNonBlocking

instance Chars.Chars L.ByteString where
  type LengthType L.ByteString = Int64
  init _ = mapM_ (\ h -> hSetBinaryMode h True) -- Prevent decoding errors when reading handles
  lazy _ = True
  length = L.length
  null = L.null
  hPutStr = L.hPutStr
  hGetContents = L.hGetContents

instance Chunks.NonBlocking L.ByteString where
  hGetNonBlocking = L.hGetNonBlocking

instance Chars.Chars T.Text where
  type LengthType T.Text = Int
  init _ _ = return ()
  lazy _ = False
  length = T.length
  null = T.null
  hPutStr = T.hPutStr
  hGetContents = T.hGetContents

instance Chars.Chars LT.Text where
  type LengthType LT.Text = Int64
  init _ _ = return ()
  lazy _ = True
  length = LT.length
  null = LT.null
  hPutStr = LT.hPutStr
  hGetContents = LT.hGetContents
