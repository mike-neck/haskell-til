{-# LANGUAGE MagicHash, TypeOperators, FlexibleContexts #-}

module Java.Nio.File.Files where

import Java
import Java.Nio.File.Path(Path)
import Java.Nio.File.FileVisitor(FileVisitor)

foreign import java unsafe "@static java.nio.file.Files.walkFileTree" walkFileTree ::
  (fv <: FileVisitor Path) => Path -> fv -> Java a Path
