{-# LANGUAGE MagicHash #-}

module Java.Io.File where

import Java

data {-# CLASS "java.io.File" #-} File = File (Object# File) deriving Class

--foreign import java unsefe "canExecute" canExecute :: Java File Bool
foreign import java unsafe "@static java.io.File.createTempFile" createTempFile:: String -> String -> Java a File
--foreign import java unsage "@new" newFile:: String -> Java a File
--foreign import java unsafe "@static @field java.io.File.pathSeparator" getPathSeparator:: Java a String
foreign import java unsafe "getAbsolutePath" getAbsolutePath :: Java File String
