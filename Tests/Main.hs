module Main where

--import Control.Applicative ((<$>))
import Test.HUnit
import System.Exit
-- import Test.Changes
-- import Test.Versions
-- import Test.VersionPolicy
-- import Test.SourcesList
-- import Test.Dependencies

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.UTF8 as B
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Prelude hiding (length, concat)
--import System.IO.Unsafe (unsafePerformIO)
import System.Process (CmdSpec(..))
import System.Process.Read (readProcessWithExitCode, readModifiedProcessWithExitCode, readModifiedProcess, readProcessChunks, keepStdout, Chars(..), discardStdout, Output(..), unpackOutputs)

main :: IO ()
main =
    do (c,st) <- runTestText putTextToShowS test1 -- (TestList (versionTests ++ sourcesListTests ++ dependencyTests ++ changesTests))
       putStrLn (st "")
       case (failures c) + (errors c) of
         0 -> return ()
         _ -> exitFailure

test1 :: Test
test1 =
    TestLabel "test1"
      (TestList
       [ TestCase (do b <- readProcessWithExitCode "Tests/Test1.hs" [] B.empty
                      assertEqual "ByteString" (ExitFailure 123, B.fromString "", B.fromString "This is an error message.\n") b)
       , TestCase (do l <- readProcessWithExitCode "Tests/Test1.hs" [] L.empty
                      assertEqual "Lazy ByteString" (ExitFailure 123, fromString "", fromString "This is an error message.\n") l)
       , TestCase (do t <- readProcessWithExitCode "Tests/Test1.hs" [] T.empty
                      assertEqual "Text" (ExitFailure 123, T.pack "", T.pack "This is an error message.\n") t)
       , TestCase (do lt <- readProcessWithExitCode "Tests/Test1.hs" [] LT.empty
                      assertEqual "LazyText" (ExitFailure 123, LT.pack "", LT.pack "This is an error message.\n") lt)
       , TestCase (do s <- readProcessWithExitCode "Tests/Test1.hs" [] ""
                      assertEqual "String" (ExitFailure 123, "", "This is an error message.\n") s)
       , TestCase (do out <- B.readFile "/usr/share/pixmaps/faces/penguin.jpg" >>=
                             readModifiedProcess id (RawCommand "djpeg" []) >>=
                             readModifiedProcess id (RawCommand "pnmfile" [])
                      assertEqual "pnmfile" (fromString "stdin:\tPPM raw, 96 by 96  maxval 255\n") out)
       , TestCase (do jpg <- B.readFile "/usr/share/pixmaps/faces/penguin.jpg"
                      (code1, pnm, err1) <- readModifiedProcessWithExitCode id (RawCommand "djpeg" []) jpg
                      out2 <- readModifiedProcess id (RawCommand "pnmfile" []) pnm
                      assertEqual "pnmfile2" (ExitSuccess, fromString "", 2192, 27661, fromString "stdin:\tPPM raw, 96 by 96  maxval 255\n") (code1, err1, length jpg, length pnm, out2))
       , TestCase (do jpg <- L.readFile "/usr/share/pixmaps/faces/penguin.jpg"
                      pnm <- readProcessChunks id (RawCommand "djpeg" []) jpg >>= return . concat . keepStdout
                      info <- readProcessChunks id (RawCommand "pnmfile" []) pnm >>= return . concat . keepStdout
                      assertEqual "pnmfile3" (fromString "stdin:\tPPM raw, 96 by 96  maxval 255\n") info)
       , TestCase (do result <- readModifiedProcessWithExitCode id (RawCommand "Tests/Test4.hs" []) "abcde"
                      assertEqual "file closed" (ExitSuccess, "a", "Read one character: 'a'\n") result)
       , TestCase (do result <- readProcessChunks id (ShellCommand "gzip -v -f < /usr/share/pixmaps/faces/penguin.jpg") L.empty
                      assertEqual "readChunks gzip" [Stderr (fromString "  2.0%\n"),Result ExitSuccess] (discardStdout result))
       , TestCase (do out <- readProcessChunks id (ShellCommand "yes | head -10 | while read i; do echo stdout; echo stderr 1>&2; done") L.empty
                      let result = unpackOutputs out
                      assertEqual "readProcessChunks stdout stderr" ([], "stdout\nstdout\nstdout\nstdout\nstdout\nstdout\nstdout\nstdout\nstdout\nstdout\n","stderr\nstderr\nstderr\nstderr\nstderr\nstderr\nstderr\nstderr\nstderr\nstderr\n", []) result) ])
