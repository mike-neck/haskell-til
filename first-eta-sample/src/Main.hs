module Main where

import Control.Monad(foldM)
import Java
import Java.Time.Month

main :: IO ()
main = do
  months <- java $ do
    ms <- monthValues
    ms <.> arrayToList
  str <- foldM joinWithComma "" months
  putStrLn str
  where
    joinWithComma :: String -> Month -> IO String
    joinWithComma [] m = return $ show m
    joinWithComma s  m = return $ s ++ ", " ++ (show m)
