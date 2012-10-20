#!/usr/bin/runhaskell

{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

import Control.Exception (try, throw, SomeException)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import System.IO (hPutStrLn, stderr)
import System.Process (CmdSpec(RawCommand))
import System.Process.ByteString as B
import System.Process.ByteString.Lazy as L
import System.Process.Read2 (Strng2, Output(..))

main =
    test1 >> test2 >> (try test3 >>= either (\ (e :: SomeException) -> hPutStrLn stderr (show e)) return)  >> test4

test1 =
    hPutStrLn stderr "=== test1 ===" >>
    B.readFile "/usr/share/pixmaps/faces/penguin.jpg" >>=
    B.readModifiedProcess id (RawCommand "djpeg" []) >>=
    B.readModifiedProcess id (RawCommand "pnmfile" []) >>= \ out ->
    putStrLn ("out: " ++ show out)

test2 =
    hPutStrLn stderr "=== test2 ===" >>
    B.readFile "/usr/share/pixmaps/faces/penguin.jpg" >>=
    B.readModifiedProcessWithExitCode id (RawCommand "djpeg" []) >>= explain >>=
    B.readModifiedProcess id (RawCommand "pnmfile" []) >>= \ out ->
    putStrLn ("out: " ++ show out)
    where
      explain (code, out, err) =
          hPutStrLn stderr ("code=" ++ show code ++ ", err=" ++ show err) >>
          return out

test3 =
    do hPutStrLn stderr "=== test3 ==="
       jpg <- L.readFile "/usr/share/pixmaps/faces/penguin.jpg"
       hPutStrLn stderr ("jpg length:" ++ show (L.length jpg))
       (code1, pnm, err1) <- L.readModifiedProcessWithExitCode id (RawCommand "djpeg" []) jpg
       hPutStrLn stderr ("pnm length: " ++ show (L.length pnm))
       pnm' <- explain (code1, pnm, err1)
       hPutStrLn stderr "pnm'"
       out <- L.readModifiedProcess id (RawCommand "pnmfile" []) pnm'
       hPutStrLn stderr "out"
       putStrLn ("out: " ++ show out)
    where
      explain (code, out, err) = do
          hPutStrLn stderr ("code=" ++ show code)
          hPutStrLn stderr ("err=" ++ show err)
          return out

test4 =
    hPutStrLn stderr "=== test4 ===" >>
    L.readFile "/usr/share/pixmaps/faces/penguin.jpg" >>=
    L.readProcessChunksWithExitCode id (RawCommand "djpeg" []) >>= mapM (explain "djpeg") >>= return . stdoutOnly >>=
    L.readProcessChunksWithExitCode id (RawCommand "pnmfile" []) >>= mapM (explain "pnmfile") >>= return . stdoutOnly >>= \ out ->
    putStrLn ("out: " ++ show out)
    where
      explain tag x@(Stderr s) = hPutStrLn stderr (tag ++ " 2> " ++ show s) >> return x
      explain tag x@(Exception e) = hPutStrLn stderr (tag ++ " e> " ++ show e) >> return x
      explain _ x = return x

-- | Filter everything except stdout from the output list.
stdoutOnly :: Strng2 L.ByteString => [Output L.ByteString] -> L.ByteString
stdoutOnly out =
    L.concat $ f out
    where 
      f (Stdout s : etc) = s : f etc
      f (_ : etc) = f etc
      f [] = []
