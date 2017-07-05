module Main where

import Control.Monad(foldM)
import Java hiding (toString)
import Java.Time.Month
import Java.Nio.File.Path
import Java.Nio.File.Files
import Java.Nio.File.FileVisitor

jstringList :: [String] -> [JString]
jstringList = fmap (toJava :: String -> JString)

jstringArray :: [String] -> Java a JStringArray
jstringArray = arrayFromList . jstringList

emptyStringArray :: Java a JStringArray
emptyStringArray = jstringArray []

main :: IO ()
main = do
  showTree

showPathOfJar :: IO ()
showPathOfJar = do
  jarFile <- java $ do
    array <- jstringArray ["build", "first-eta-sample", "first-eta-sample.jar"]
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

showTree :: IO ()
showTree = java $ do
  dir <- emptyStringArray >>= (getPath "dist")
  walkFileTree dir newPathVisitor >> return ()
