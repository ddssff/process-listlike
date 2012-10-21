-- | Some functions brought over from my obsolete progress packages.
{-# LANGUAGE RankNTypes, ScopedTypeVariables, TypeFamilies #-}
module System.Process.Read.Compat
    ( prefixes
    , echo
    ) where

import qualified Data.ByteString.Lazy.Char8 as L
import System.Process (CmdSpec(..), showCommandForUser)
import System.Process.Read.Convenience (ePutStrLn)
import System.Process.Read.Chunks (Output(..))

-- | Return the original stream of outputs zipped with one that has
-- had prefixes for stdout and stderr inserted.  For the prefixed
-- stream only, apply @map snd@.
prefixes :: forall a. (a ~ L.ByteString) => a -> a -> [Output a] -> [(Output a, Output a)]
prefixes opre epre output =
    f True output
    where
      f :: Bool -> [Output a] -> [(Output a, Output a)]
      f _ [] = []
      f bol (x@(Stdout s) : xs) = let (s', bol') = doOutput bol opre s in (x, Stdout s') : f bol' xs
      f bol (x@(Stderr s) : xs) = let (s', bol') = doOutput bol epre s in (x, Stderr s') : f bol' xs
      f bol (x : xs) = (x, Stdout L.empty) : f bol xs

doOutput :: forall a. (a ~ L.ByteString) => Bool -> a -> a -> (a, Bool)
doOutput bol pre s =
    let (a, b) = L.span (/= '\n') s in
    if L.null a
    then if L.null b
         then (L.empty, bol)
         else let x = (if bol then pre else L.empty)
                  (s', bol') = doOutput True pre (L.tail b) in
              (L.concat [x, (L.pack "\n"), s'], bol')
          -- There is some text before a possible newline
    else let x = (if bol then L.append pre a else a)
             (s', bol') = doOutput False pre b in
         (L.append x s', bol')

echo :: CmdSpec -> IO () -> IO ()
echo (RawCommand cmd args) io = ePutStrLn ("-> " ++ showCommandForUser cmd args) >> io
echo (ShellCommand cmd) io = ePutStrLn ("-> " ++ cmd) >> io
