{-# LANGUAGE MagicHash #-}

module Java.Util.Function where

import Java
import Prelude hiding (Int)

-- ---- import java.util.function.IntFunction
data {-# CLASS "java.util.function.IntFunction" #-} IntFunction a = IntFunction (Object# (IntFunction a)) deriving Class

foreign import java unsafe "@wrapper apply"
  intFunction :: (a <: Object) => (Int -> Java (IntFunction a) a) -> IntFunction a

-- ---- import java.util.function.Function
data {-# CLASS "java.util.function.Function" #-} Function t r = Function (Object# (Function t r))
  deriving Class

foreign import java unsafe "@wrapper apply"
  mkFunction :: (t <: Object, r <: Object) => (t -> Java (Function t r) r) -> Function t r
