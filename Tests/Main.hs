module Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.UTF8 as B
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Prelude hiding (length, concat)
import System.Exit
import System.Posix.Files (getFileStatus, fileMode, setFileMode, unionFileModes, ownerExecuteMode, groupExecuteMode, otherExecuteMode)
import System.Process (CmdSpec(..))
import System.Process.Read (readProcessWithExitCode, readModifiedProcessWithExitCode, readModifiedProcess, Chars(..))
import Test.HUnit hiding (path)

main :: IO ()
main =
    do chmod "Tests/Test1.hs"
       chmod "Tests/Test2.hs"
       chmod "Tests/Test4.hs"
       (c,st) <- runTestText putTextToShowS test1 -- (TestList (versionTests ++ sourcesListTests ++ dependencyTests ++ changesTests))
       putStrLn (st "")
       case (failures c) + (errors c) of
         0 -> return ()
         _ -> exitFailure

chmod :: FilePath -> IO ()
chmod path =
    getFileStatus "Tests/Test1.hs" >>= \ status ->
    setFileMode path (foldr unionFileModes (fileMode status) [ownerExecuteMode, groupExecuteMode, otherExecuteMode])

test1 :: Test
test1 =
    TestLabel "test1"
      (TestList
       [ TestLabel "ByteString" $
         TestCase (do b <- readProcessWithExitCode "Tests/Test1.hs" [] B.empty
                      assertEqual "ByteString" (ExitFailure 123, B.fromString "", B.fromString "This is an error message.\n") b)
       , TestLabel "Lazy" $
         TestCase (do l <- readProcessWithExitCode "Tests/Test1.hs" [] L.empty
                      assertEqual "Lazy ByteString" (ExitFailure 123, fromString "", fromString "This is an error message.\n") l)
       , TestLabel "Text" $
         TestCase (do t <- readProcessWithExitCode "Tests/Test1.hs" [] T.empty
                      assertEqual "Text" (ExitFailure 123, T.pack "", T.pack "This is an error message.\n") t)
       , TestLabel "LazyText" $
         TestCase (do lt <- readProcessWithExitCode "Tests/Test1.hs" [] LT.empty
                      assertEqual "LazyText" (ExitFailure 123, LT.pack "", LT.pack "This is an error message.\n") lt)
       , TestLabel "String" $
         TestCase (do s <- readProcessWithExitCode "Tests/Test1.hs" [] ""
                      assertEqual "String" (ExitFailure 123, "", "This is an error message.\n") s)
       , TestLabel "pnmfile" $
         TestCase (do out <- B.readFile "Tests/penguin.jpg" >>=
                             readModifiedProcess id (RawCommand "djpeg" []) >>=
                             readModifiedProcess id (RawCommand "pnmfile" [])
                      assertEqual "pnmfile" (fromString "stdin:\tPPM raw, 96 by 96  maxval 255\n") out)
       , TestLabel "pnmfile2" $
         TestCase (do jpg <- B.readFile "Tests/penguin.jpg"
                      (code1, pnm, err1) <- readModifiedProcessWithExitCode id (RawCommand "djpeg" []) jpg
                      out2 <- readModifiedProcess id (RawCommand "pnmfile" []) pnm
                      assertEqual "pnmfile2" (ExitSuccess, fromString "", 2192, 27661, fromString "stdin:\tPPM raw, 96 by 96  maxval 255\n") (code1, err1, length jpg, length pnm, out2))
       , TestLabel "file closed 1" $
         TestCase (do result <- readModifiedProcessWithExitCode id (RawCommand "Tests/Test4.hs" []) (fromString "a" :: B.ByteString)
                      assertEqual "file closed 1" (ExitSuccess, (fromString "a"), (fromString "Read one character: 'a'\n")) result)
       , TestLabel "file closed 2" $
         TestCase (do result <- readModifiedProcessWithExitCode id (RawCommand "Tests/Test4.hs" []) (fromString "" :: L.ByteString)
                      assertEqual "file closed 2" (ExitFailure 1, empty, (fromString "Test4.hs: <stdin>: hGetChar: end of file\n")) result)
       , TestLabel "file closed 3" $
         TestCase (do result <- readModifiedProcessWithExitCode id (RawCommand "Tests/Test4.hs" []) "abcde"
                      assertEqual "file closed 3" (ExitSuccess, "a", "Read one character: 'a'\n") result)
       , TestLabel "file closed 4" $
         TestCase (do result <- readModifiedProcessWithExitCode id (RawCommand "Tests/Test4.hs" []) "abcde"
                      assertEqual "file closed 4" (ExitSuccess, "a", "Read one character: 'a'\n") result)
       ])
