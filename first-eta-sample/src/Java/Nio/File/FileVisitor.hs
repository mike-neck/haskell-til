{-# LANGUAGE MagicHash, TypeFamilies, DataKinds, FlexibleContexts, TypeOperators #-}

module Java.Nio.File.FileVisitor where

import Java
import Java.Nio.File.Path(Path, toAbsolutePath)

data {-# CLASS "java.nio.file.FileVisitor" #-} FileVisitor a = FileVisitor (Object# (FileVisitor a)) deriving Class

data {-# CLASS "com.example.PathVisitor implements java.nio.file.FileVisitor<java.nio.file.Path>" #-} PathVisitor
  = PathVisitor (Object# PathVisitor) deriving Class

type instance Inherits PathVisitor = '[FileVisitor Path]

foreign import java unsafe "@new" newPathVisitor :: PathVisitor

data {-# CLASS "java.nio.file.attribute.BasicFileAttributes" #-} BasicFileAttributes =
  BasicFileAttributes (Object# BasicFileAttributes) deriving Class

data {-# CLASS "java.nio.file.FileVisitResult" #-} FileVisitResult =
  FileVisitResult (Object# FileVisitResult) deriving (Class, Eq, Show)

foreign import java unsafe "@static @field java.nio.file.FileVisitResult.CONTINUE" fileVisitContinue :: FileVisitResult

visitFile :: (v <: (FileVisitor Path)) => Path -> BasicFileAttributes -> Java v FileVisitResult
visitFile p _ = do
  path <- p <.> toAbsolutePath
  io $ putStrLn $ show path
  return fileVisitContinue

data {-# CLASS "java.io.IOException" #-} IOException = IOException (Object# IOException) deriving Class

justContinue :: (v <: (FileVisitor Path)) => Java v FileVisitResult
justContinue = return fileVisitContinue

visitFileFailed :: (v <: (FileVisitor Path)) => Path -> IOException -> Java v FileVisitResult
visitFileFailed _ _ = justContinue

preVisitDirectory :: (v <: (FileVisitor Path)) => Path -> BasicFileAttributes -> Java v FileVisitResult
preVisitDirectory _ _ = justContinue

postVisitDirectory :: (v <: (FileVisitor Path)) => Path -> IOException -> Java v FileVisitResult
postVisitDirectory _ _ = justContinue

foreign export java "visitFile" visitFile :: Path -> BasicFileAttributes -> Java PathVisitor FileVisitResult
foreign export java "visitFileFailed" visitFileFailed :: Path -> IOException -> Java PathVisitor FileVisitResult
foreign export java "preVisitDirectory"
  preVisitDirectory :: Path -> BasicFileAttributes -> Java PathVisitor FileVisitResult
foreign export java "postVisitDirectory" postVisitDirectory :: Path -> IOException -> Java PathVisitor FileVisitResult
