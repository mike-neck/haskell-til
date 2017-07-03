{-# LANGUAGE MagicHash #-}

module Java.Time.Month where

import Java
import Prelude hiding(Enum)

data {-# CLASS "java.lang.Enum" #-} Enum a = Enum (Object# (Enum a)) deriving Class

data {-# CLASS "java.time.Month" #-} Month = Month (Object# Month) deriving (Class, Show, Eq)

data {-# CLASS "java.time.Month[]" #-} MonthArray = MonthArray (Object# MonthArray) deriving Class

instance JArray Month MonthArray

type instance Inherits Month = '[Enum Month]

foreign import java unsafe "@static java.time.Month.values" monthValues :: Java a MonthArray
