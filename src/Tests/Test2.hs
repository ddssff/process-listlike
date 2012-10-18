#!/usr/bin/runhaskell

import System.Process.String (readProcessWithExitCode)

main =
    do (code, out, err) <- readProcessWithExitCode "./Test1.hs" [] ""
       putStrLn ("code=" ++ show code ++ "\nout=" ++ show out ++ "\nerr=" ++ show err)
