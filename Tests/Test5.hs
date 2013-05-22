#!/usr/bin/runhaskell

import System.Process (CmdSpec(RawCommand))
import System.Process.Read (readCreateProcessWithExitCode)

main =
    do (code, out, err) <- readCreateProcessWithExitCode id (RawCommand "Tests/Test4.hs" []) "abcde"
       putStrLn ("code=" ++ show code ++ "\nout=" ++ show out ++ "\nerr=" ++ show err)
