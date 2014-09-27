{-# LANGUAGE ScopedTypeVariables #-}
-- | Tests that require a user to send a keyboard interrupt.
module Main where

import qualified Data.Text.Lazy
import qualified Data.ByteString.Lazy
import Data.Monoid
import Debug.Console (ePutStrLn)
import System.Process
import System.Process.ListLike.Chunks (Chunk)
import qualified System.Process.ListLike.Class as Class
import System.Process.ListLike.Instances ()
import qualified System.Process.ListLike.Ready as Ready
import qualified System.Process.ListLike.Thread as Thread
import System.Environment
import System.IO
import Text.Read (readMaybe)

main :: IO ()
main = do
  args <- getArgs
  case readArgs args of
    Nothing ->
        hPutStrLn stderr $
          unlines ([ "usage: interactive-tests test# runner#"
                   , "tests: " ] ++
                   map (\ (n, test) -> " " ++ show n ++ ". " ++ test) (zip ([1..] :: [Int]) tests) ++
                   [ "runners: " ] ++
                   map (\ (n, (runner, _)) -> " " ++ show n ++ ". " ++ runner) (zip ([1..] :: [Int]) runners))
    Just (test, (s, runner)) ->
        do ePutStrLn (test ++ " -> " ++ s)
           runner (shell test)
    where
      readArgs :: [String] -> Maybe (String, (String, CreateProcess -> IO ()))
      readArgs [t, r] =
          case (readMaybe t, readMaybe r) of
            (Just t', Just r')
                | t' >= 1 && t' <= length tests && r' >= 1 && r' <= length runners ->
                    Just (tests !! (t' - 1), runners !! (r' - 1))
            _ -> Nothing
      readArgs _ = Nothing

tests :: [String]
tests = [ "ls -l /tmp"
        , "yes | cat -n | while read i; do echo $i; sleep 1; done"
        , "oneko"
        , "yes | cat -n" ]

runners  :: [(String, CreateProcess -> IO ())]
runners = [ ("Ready.readProcessInterleaved Lazy.Text",
             \ p -> Ready.readProcessInterleaved p mempty >>= \ (b :: [Chunk Data.Text.Lazy.Text]) ->
                    mapM_ (ePutStrLn . show) b)
          , ("Ready.readProcessInterleaved Lazy.ByteString",
             \ p -> Ready.readProcessInterleaved p mempty >>= \ (b :: [Chunk Data.ByteString.Lazy.ByteString]) ->
                    mapM_ (ePutStrLn . show) b)
          , ("Thread.readProcessInterleaved Lazy.Text",
             \ p -> Thread.readProcessInterleaved p mempty >>= \ (b :: [Chunk Data.Text.Lazy.Text]) ->
                    mapM_ (ePutStrLn . show) b)
          , ("Thread.readProcessInterleaved Lazy.ByteString",
             \ p -> Thread.readProcessInterleaved p mempty >>= \ (b :: [Chunk Data.ByteString.Lazy.ByteString]) ->
                    mapM_ (ePutStrLn . show) b)
          ]
