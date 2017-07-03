{-# LANGUAGE MagicHash #-}

module Java.Util.Stream where

import Java
import Java.Util.Function


-- ---- import java.util.stream.Stream
data {-# CLASS "java.util.stream.Stream" #-} Stream a = Stream (Object# (Stream a)) deriving Class



-- ---- import java.util.stream.IntStream
data {-# CLASS "java.util.stream.IntStream" #-} IntStream = IntStream (Object# IntStream) deriving Class

foreign import java unsafe "@static java.util.stream.IntStream.of" intStreamOf :: JIntArray -> Java a IntStream

foreign import java unsafe "mapToObj" intMapToObj :: IntFunction a -> Java IntStream (Stream a)


