{-# LANGUAGE MagicHash, TypeOperators, FlexibleContexts #-}

module Java.Nio.File.Path where

import Java

data {-# CLASS "java.nio.file.Path" #-} Path = Path (Object# Path) deriving (Class, Show, Eq)

foreign import java unsafe "@static java.nio.file.Paths.get" getPath :: String -> JStringArray -> Java a Path

foreign import java unsafe "@interface toAbsolutePath" toAbsolutePath :: Java Path Path

foreign import java unsafe "@interface toString" toString :: (a <: Path) => Java a String
