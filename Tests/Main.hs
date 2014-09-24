{-# LANGUAGE FlexibleInstances, OverloadedStrings, RankNTypes, ScopedTypeVariables #-}
module Main where

import Codec.Binary.UTF8.String (encode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.ListLike as ListLike (ListLike(..))
import Data.Maybe (mapMaybe)
import Data.Monoid (Monoid(..), (<>))
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Prelude hiding (length, concat, null)
import GHC.IO.Exception
import System.Exit
import System.Posix.Files (getFileStatus, fileMode, setFileMode, unionFileModes, ownerExecuteMode, groupExecuteMode, otherExecuteMode)
import System.Process (proc, shell)
import System.Process.ListLike (readProcessWithExitCode, readCreateProcessWithExitCode, readCreateProcess, ListLikePlus(..), Chunk(..), readProcessChunks)
import Test.HUnit hiding (path)

-- | Merge adjacent and eliminate empty Stdout or Stderr chunks.  This
-- may not be a good idea if we are looking to get our output as soon
-- as it becomes available.
canonicalChunks :: ListLikePlus a c => [Chunk a] -> [Chunk a]
canonicalChunks [] = []
canonicalChunks (Stdout a : Stdout b : more) = canonicalChunks (Stdout (a <> b) : more)
canonicalChunks (Stderr a : Stderr b : more) = canonicalChunks (Stderr (a <> b) : more)
canonicalChunks (Stdout a : more) | null a = canonicalChunks more
canonicalChunks (Stderr a : more) | null a = canonicalChunks more
canonicalChunks (a : more) = a : canonicalChunks more

fromString :: String -> B.ByteString
fromString = fromList . encode

lazyFromString :: String -> L.ByteString
lazyFromString = L.fromChunks . (: []) . fromString

instance Monoid Test where
    mempty = TestList []
    mappend (TestList a) (TestList b) = TestList (a ++ b)
    mappend (TestList a) b = TestList (a ++ [b])
    mappend a (TestList b) = TestList ([a] ++ b)
    mappend a b = TestList [a, b]

testInstances :: (forall a c. (Show a, ListLikePlus a c) => a -> Test) -> Test
testInstances mkTest = mappend (testCharInstances mkTest) (testWord8Instances mkTest)

testStrictInstances :: (forall a c. (Show a, ListLikePlus a c) => a -> Test) -> Test
testStrictInstances mkTest = mappend (testStrictCharInstances mkTest) (testStrictWord8Instances mkTest)

testLazyInstances :: (forall a c. (Show a, ListLikePlus a c) => a -> Test) -> Test
testLazyInstances mkTest = mappend (testLazyCharInstances mkTest) (testLazyWord8Instances mkTest)

testCharInstances :: (forall a c. (Show a, ListLikePlus a c) => a -> Test) -> Test
testCharInstances mkTest = mappend (testLazyCharInstances mkTest) (testStrictCharInstances mkTest)

testLazyCharInstances :: (forall a c. (Show a, ListLikePlus a c) => a -> Test) -> Test
testLazyCharInstances mkTest =
    TestList [ TestLabel "Lazy Text" $ mkTest LT.empty
             , TestLabel "String" $ mkTest ("" :: String) ]

testStrictCharInstances :: (forall a c. (Show a, ListLikePlus a c) => a -> Test) -> Test
testStrictCharInstances mkTest =
    TestList [ TestLabel "Strict Text" $ mkTest T.empty ]

testWord8Instances :: (forall a c. (Show a, ListLikePlus a c) => a -> Test) -> Test
testWord8Instances mkTest = mappend (testLazyWord8Instances mkTest) (testStrictWord8Instances mkTest)

testLazyWord8Instances :: (forall a c. (Show a, ListLikePlus a c) => a -> Test) -> Test
testLazyWord8Instances mkTest =
    TestList [ TestLabel "Lazy ByteString" $ mkTest L.empty ]

testStrictWord8Instances :: (forall a c. (Show a, ListLikePlus a c) => a -> Test) -> Test
testStrictWord8Instances mkTest =
    TestList [ TestLabel "Strict ByteString" $ mkTest B.empty ]

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
       [ TestLabel "[Output]" $
         TestList
         [ testCharInstances
           (\ i -> TestCase (do b <- readProcessChunks (proc "cat" ["Tests/text"]) i
                                assertEqual
                                    "UTF8"
                                    ["ProcessHandle <processhandle>",
                                     -- For Text, assuming your locale is set to utf8, the result is unicode.
                                     "Stdout \"Signed: Baishi laoren \\30333\\30707\\32769\\20154, painted in the artist\\8217s 87th year.\\n\"",
                                     "Result ExitSuccess"] (ListLike.map show (canonicalChunks b))))
         , testWord8Instances
           (\ i -> TestCase (do b <- readProcessChunks (proc "cat" ["Tests/text"]) i
                                assertEqual
                                    "UTF8"
                                    ["ProcessHandle <processhandle>",
                                     -- For ByteString we get utf8 encoded text
                                     "Stdout \"Signed: Baishi laoren \\231\\153\\189\\231\\159\\179\\232\\128\\129\\228\\186\\186, painted in the artist\\226\\128\\153s 87th year.\\n\"",
                                     "Result ExitSuccess"] (ListLike.map show (canonicalChunks b))))
         , testInstances
           (\ i -> TestCase (do b <- readProcessChunks (proc "Tests/Test1.hs" []) i
                                assertEqual
                                    "[Chunk]"
                                    ["ProcessHandle <processhandle>",
                                     "Stderr \"This is an error message.\\n\"",
                                     "Result (ExitFailure 123)"]
                                    (ListLike.map show (canonicalChunks b))))
       ]
       -- This gets "hGetContents: invalid argument (invalid byte sequence)" if we don't call
       -- binary on the file handles in readProcessInterleaved.

       , TestLabel "JPG" $
         TestList
         [ {- TestCase (do b <- readProcessChunks (proc "cat" ["Tests/houseisclean.jpg"]) B.empty >>=
                             return . mapMaybe (\ x -> case x of
                                                         Stdout s -> Just (length' s)
                                                         Stderr s -> Just (length' s)
                                                         _ -> Nothing)
                        assertEqual "ByteString Chunk Size"
                                    [68668,0]
                                    b
                                    -- If we could read a jpg file into a string the chunks would look something like this:
                                    -- [2048,2048,2048,1952,2048,2048,2048,1952,2048,2048,2048,1952,2048,2048,2048,1952,2048,2048,2048,1952,2048,2048,2048,1952,2048,2048,2048,1952,2048,2048,2048,1952,2048,1852]
                                    )
         , -}
           testLazyWord8Instances
           ( \ i -> TestCase (do b <- readProcessChunks (proc "cat" ["Tests/houseisclean.jpg"]) i >>=
                                      return . mapMaybe (\ x -> case x of
                                                                  Stdout s -> Just (length s)
                                                                  Stderr s -> Just (length s)
                                                                  _ -> Nothing)
                                 assertEqual "Chunk Size" [32752,32752,3164] b))
         , testStrictWord8Instances
           ( \ i -> TestCase (do b <- readProcessChunks (proc "cat" ["Tests/houseisclean.jpg"]) i >>=
                                      return . mapMaybe (\ x -> case x of
                                                                  Stdout s -> Just (length s)
                                                                  Stderr s -> Just (length s)
                                                                  _ -> Nothing)
                                 assertEqual "Chunk Size" [68668,0] b))
{-       -- We don't seem to get an InvalidArgument exception back.
         , TestCase (do b <- tryIOError (readProcessChunks (proc "cat" ["Tests/houseisclean.jpg"]) "") >>= return . either Left (Right . show)
                        assertEqual "String decoding exception" (Left (IOError { ioe_handle = Nothing
                                                                               , ioe_type = InvalidArgument
                                                                               , ioe_location = "recoverDecode"
                                                                               , ioe_description = "invalid byte sequence"
                                                                               , ioe_errno = Nothing
                                                                               , ioe_filename = Nothing })) b)

Related to https://ghc.haskell.org/trac/ghc/ticket/9236.  Try this:

  import System.IO
  import System.IO.Error

  main = do
    h <- openFile "Tests/houseisclean.jpg" ReadMode
    r <- try (hGetContents h) >>= either exn str
    hClose h
      where
        exn (e :: IOError) = putStrLn ("exn=" ++ show (ioe_handle e, ioe_type e, ioe_location e, ioe_description e, ioe_errno e, ioe_filename e))
        str s = putStrLn ("s=" ++ show s)

The exception gets thrown and caught after the string result starts
being printed.  You can see the open quote.
-}
         ]
{-
       , TestLabel "ByteString" $
         TestCase (do b <- readProcessWithExitCode "Tests/Test1.hs" [] B.empty
                      assertEqual "ByteString" (ExitFailure 123, fromString "", fromString "This is an error message.\n") b)
-}
       , TestLabel "Lazy" $
         TestCase (do l <- readProcessWithExitCode "Tests/Test1.hs" [] L.empty
                      assertEqual "Lazy ByteString" (ExitFailure 123, lazyFromString "", lazyFromString "This is an error message.\n") l)
{-
       , TestLabel "Text" $
         TestCase (do t <- readProcessWithExitCode "Tests/Test1.hs" [] T.empty
                      assertEqual "Text" (ExitFailure 123, T.pack "", T.pack "This is an error message.\n") t)
-}
       , TestLabel "LazyText" $
         TestCase (do lt <- readProcessWithExitCode "Tests/Test1.hs" [] LT.empty
                      assertEqual "LazyText" (ExitFailure 123, LT.pack "", LT.pack "This is an error message.\n") lt)
{-
       , TestLabel "String" $
         TestCase (do s <- readProcessWithExitCode "Tests/Test1.hs" [] ""
                      assertEqual "String" (ExitFailure 123, "", "This is an error message.\n") s)
-}
       , TestLabel "pnmfile" $
         TestCase (do out <- L.readFile "Tests/penguin.jpg" >>=
                             readCreateProcess (proc "djpeg" []) >>=
                             readCreateProcess (proc "pnmfile" [])
                      assertEqual "pnmfile" (lazyFromString "stdin:\tPPM raw, 96 by 96  maxval 255\n") out)
       , TestLabel "pnmfile2" $
         TestCase (do jpg <- L.readFile "Tests/penguin.jpg"
                      (code1, pnm, err1) <- readCreateProcessWithExitCode (proc "djpeg" []) jpg
                      out2 <- readCreateProcess (proc "pnmfile" []) pnm
                      assertEqual "pnmfile2" (ExitSuccess, empty, 2192, 27661, lazyFromString "stdin:\tPPM raw, 96 by 96  maxval 255\n") (code1, err1, length jpg, length pnm, out2))
       , TestLabel "file closed 1" $
         TestCase (do result <- readCreateProcessWithExitCode (proc "Tests/Test4.hs" []) (lazyFromString "a" :: L.ByteString)
                      assertEqual "file closed 1" (ExitSuccess, (lazyFromString "a"), (lazyFromString "Read one character: 'a'\n")) result)
       , TestLabel "file closed 2" $
         TestCase (do result <- readCreateProcessWithExitCode (proc "Tests/Test4.hs" []) (lazyFromString "" :: L.ByteString)
                      assertEqual "file closed 2" (ExitFailure 1, empty, (lazyFromString "Test4.hs: <stdin>: hGetChar: end of file\n")) result)
       , TestLabel "file closed 3" $
         TestCase (do result <- readCreateProcessWithExitCode (proc "Tests/Test4.hs" []) ("abcde" :: LT.Text)
                      assertEqual "file closed 3" (ExitSuccess, "a", "Read one character: 'a'\n") result)
       , TestLabel "file closed 4" $
         TestCase (do result <- readCreateProcessWithExitCode (proc "Tests/Test4.hs" []) ("abcde" :: LT.Text)
                      assertEqual "file closed 4" (ExitSuccess, "a", "Read one character: 'a'\n") result)
       , TestLabel "exit code 0" $
         TestCase (do result <- readCreateProcess (shell "echo \"hello, world\"") ("" :: LT.Text)
                      assertEqual "exit code 1" "\"hello, world\\n\"" (show result)
                  )
       -- I'd like to test readCreateProcess but I can't seem to catch the exception it throws
       , TestLabel "exit code 1" $
         TestCase (do result <- readCreateProcessWithExitCode (proc "bash" ["-e", "exit", "1"]) ("" :: LT.Text)
                      assertEqual "exit code 1" "(ExitFailure 1,\"\",\"bash: exit: No such file or directory\\n\")" (show result)
                  )
       ])
