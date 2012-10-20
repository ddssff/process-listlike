#!/usr/bin/runhaskell

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import System.Process.Read (readProcessWithExitCode)

main =
    do (code, out, err) <- readProcessWithExitCode "Tests/Test1.hs" [] B.empty
       putStrLn ("code=" ++ show code ++ "\nout=" ++ show out ++ "\nerr=" ++ show err ++ "\n")
       (code, out, err) <- readProcessWithExitCode "Tests/Test1.hs" [] L.empty
       putStrLn ("code=" ++ show code ++ "\nout=" ++ show out ++ "\nerr=" ++ show err ++ "\n")
       (code, out, err) <- readProcessWithExitCode "Tests/Test1.hs" [] T.empty
       putStrLn ("code=" ++ show code ++ "\nout=" ++ show out ++ "\nerr=" ++ show err ++ "\n")
       (code, out, err) <- readProcessWithExitCode "Tests/Test1.hs" [] LT.empty
       putStrLn ("code=" ++ show code ++ "\nout=" ++ show out ++ "\nerr=" ++ show err ++ "\n")
       (code, out, err) <- readProcessWithExitCode "Tests/Test1.hs" [] ""
       putStrLn ("code=" ++ show code ++ "\nout=" ++ show out ++ "\nerr=" ++ show err)
