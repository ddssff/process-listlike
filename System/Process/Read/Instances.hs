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
import System.Process.Read.Chunks (NonBlocking(..))

instance Chars.Chars String where
  type LengthType String = Int
  init _ = mapM_ (\ h -> hSetBinaryMode h True) -- Prevent decoding errors when reading handles (because internally this uses lazy bytestrings)
  lazy _ = True
  length = length
  null = null
  append = (++)
  concat = concat
  fromString = id
  toString = id
  empty = ""
  hPutStr h s = L.hPutStr h (L.fromChunks [fromString s])
  hGetContents h = (toString . B.concat . L.toChunks) <$> L.hGetContents h

instance Chars.Chars B.ByteString where
  type LengthType B.ByteString = Int
  init _ = mapM_ (\ h -> hSetBinaryMode h True) -- Prevent decoding errors when reading handles
  lazy _ = False
  length = B.length
  null = B.null
  append = B.append
  concat = B.concat
  fromString = fromString
  toString = toString
  empty = B.empty
  hPutStr = B.hPutStr
  hGetContents = B.hGetContents

instance Chars.Chars L.ByteString where
  type LengthType L.ByteString = Int64
  init _ = mapM_ (\ h -> hSetBinaryMode h True) -- Prevent decoding errors when reading handles
  lazy _ = True
  length = L.length
  null = L.null
  append = L.append
  concat = L.concat
  fromString = L.fromChunks . (: []) . fromString
  toString = toString . B.concat . L.toChunks
  empty = L.empty
  hPutStr = L.hPutStr
  hGetContents = L.hGetContents

instance Chars.Chars T.Text where
  type LengthType T.Text = Int
  init _ _ = return ()
  lazy _ = False
  length = T.length
  null = T.null
  append = T.append
  concat = T.concat
  fromString = T.pack
  toString = T.unpack
  empty = T.empty
  hPutStr = T.hPutStr
  hGetContents = T.hGetContents

instance Chars.Chars LT.Text where
  type LengthType LT.Text = Int64
  init _ _ = return ()
  lazy _ = True
  length = LT.length
  null = LT.null
  append = LT.append
  concat = LT.concat
  fromString = LT.pack
  toString = LT.unpack
  empty = LT.empty
  hPutStr = LT.hPutStr
  hGetContents = LT.hGetContents

instance NonBlocking String where
  hGetNonBlocking n h = (toString . B.concat . L.toChunks) <$> L.hGetNonBlocking n h
  hGetSome h n = toString <$> B.hGetSome h n
  toChunks = error "toChunks"

instance NonBlocking B.ByteString where
  hGetNonBlocking = B.hGetNonBlocking
  hGetSome = B.hGetSome
  toChunks = (: [])

instance NonBlocking L.ByteString where
  hGetNonBlocking = L.hGetNonBlocking
  hGetSome h n = (L.fromChunks . (: [])) <$> B.hGetSome h (fromIntegral n)
  toChunks = map (L.fromChunks . (: [])) . L.toChunks
