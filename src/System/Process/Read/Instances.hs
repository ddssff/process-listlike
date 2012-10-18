{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module System.Process.Read.Instances where

import Control.Applicative ((<$>))
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.ByteString.UTF8 (toString)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import System.IO (hSetBinaryMode, hPutStr, hGetContents)
import qualified System.Process.Read.Chars as Chars
import qualified System.Process.Read.Chunks as Chunks

instance Chars.Chars String where
  init _ = mapM_ (\ h -> hSetBinaryMode h True) -- Prevent decoding errors when reading handles (because internally this uses lazy bytestrings)
  lazy _ = False
  length = fromInteger . toInteger . length
  null = null
  hPutStr = hPutStr
  hGetContents = hGetContents

instance Chunks.NonBlocking String where
  hGetNonBlocking n h = (toString . B.concat . L.toChunks) <$> L.hGetNonBlocking n h

instance Chars.Chars B.ByteString where
  init _ = mapM_ (\ h -> hSetBinaryMode h True) -- Prevent decoding errors when reading handles
  lazy _ = False
  length = fromInteger . toInteger . B.length
  null = B.null
  hPutStr = B.hPutStr
  hGetContents = B.hGetContents

instance Chunks.NonBlocking B.ByteString where
  hGetNonBlocking = B.hGetNonBlocking

instance Chars.Chars L.ByteString where
  init _ = mapM_ (\ h -> hSetBinaryMode h True) -- Prevent decoding errors when reading handles
  lazy _ = True
  length = L.length
  null = L.null
  hPutStr = L.hPutStr
  hGetContents = L.hGetContents

instance Chunks.NonBlocking L.ByteString where
  hGetNonBlocking = L.hGetNonBlocking

instance Chars.Chars T.Text where
  init _ _ = return () 
  lazy _ = False
  length = fromInteger . toInteger . T.length
  null = T.null
  hPutStr = T.hPutStr
  hGetContents = T.hGetContents

instance Chars.Chars LT.Text where
  init _ _ = return ()
  lazy _ = True
  length = LT.length
  null = LT.null
  hPutStr = LT.hPutStr
  hGetContents = LT.hGetContents
