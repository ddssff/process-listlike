#!/usr/bin/runhaskell

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.UTF8 as B
import qualified Data.ByteString.Lazy.UTF8 as L
import Data.Char (ord)
import Data.Monoid (mempty)
import qualified Data.Text.Lazy as L (Text)
import System.IO (hPutStr, stderr, hPutStrLn)
import System.Process -- (CmdSpec(RawCommand, ShellCommand), rawSystem)
import System.Process.Chunks (readProcessChunks, dotifyChunks, putDots, putChunk)

main :: IO ()
main =
    readProcessChunks (shell "rsync -aHxS -v dsf@src.seereason.com:/srv/darcs/public/process-extras/ /tmp/process-extras 1>&2") (L.fromString "") >>=
    mapM_ putChunk
    -- putDots 100 (fromIntegral (ord '.')) >> return ()

-- Test infinite strings for each of the ListLikePlus instances

-- Never exits - on my machine I get about 1 dot per second at 50M chars per dot.
test2 :: IO ()
test2 =
   readProcessChunks (proc "/usr/bin/yes" []) (mempty :: L.ByteString) >>=
   putDots 50000000 (fromIntegral (ord '.')) >> return ()

-- Hangs - only lazy types work
test3 :: IO ()
test3 =
   readProcessChunks (proc "/usr/bin/yes" []) (mempty :: B.ByteString) >>=
   putDots 50000000 (fromIntegral (ord '.')) >> return ()

-- Dies after one chunk (2048 characters)
test4 :: IO ()
test4 =
   readProcessChunks (proc "/usr/bin/yes" []) ("" :: String) >>=
   mapM_ (putStrLn . show)
   -- putDots 1 '.' >> return ()

-- One dot every 10 million chars - about 1/4 the speed of lazy bytestring.
test5 :: IO ()
test5 =
   readProcessChunks (proc "/usr/bin/yes" []) (mempty :: L.Text) >>=
   -- mapM_ (putStrLn . show)
   putDots 10000000 '.' >> return ()
