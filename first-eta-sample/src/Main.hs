module Main where

import Control.Monad(foldM)
import Java hiding (toString)
import Java.Time.Month
import Java.Nio.File.Path

main :: IO ()
main = do
  showPathOfJar

showPathOfJar :: IO ()
showPathOfJar = do
  jarFile <- java $ do
    array <- arrayFromList $ fmap (toJava :: String -> JString) ["build", "first-eta-sample", "first-eta-sample.jar"]
    path <- getPath "dist" array
    path <.> toAbsolutePath
  pathString <- java $ jarFile <.> toString
  putStrLn pathString

showMonths :: IO ()
showMonths = do
  months <- java $ do
    ms <- monthValues
    ms <.> arrayToList
  str <- foldM joinWithComma "" months
  putStrLn str
  where
    joinWithComma :: String -> Month -> IO String
    joinWithComma [] m = return $ show m
    joinWithComma s  m = return $ s ++ ", " ++ (show m)
