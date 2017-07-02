module Main where

import Java.Io.File
import Java

main :: IO ()
main = do
  path <- java $ do
    file <- createTempFile "sample" ".txt"
    io $ putStrLn "Executing an IO action inside of Java Monad"
    file <.> getAbsolutePath
  putStrLn path
