module Ch2.Persistent.List where

import Prelude hiding (tail)

class Stack s where
  empty:: s a
  isEmpty:: s a -> Bool
  cons:: a -> s a -> s a
  head:: s a -> a
  tail:: s a -> s a

instance Stack [] where
  empty = []

  isEmpty [] = True
  isEmpty _  = False

  cons x xs = x : xs

  head [] = error "It's empty."
  head (x:_) = x

  tail [] = error "It's empty."
  tail (_:xs) = xs

data CustomStack a = Nil | Cons a (CustomStack a)

instance Stack CustomStack where
  empty = Nil

  isEmpty Nil = True
  isEmpty _   = False

  cons x xs = Cons x xs

  head Nil          = error "It's empty."
  head (Cons x _)   = x

  tail Nil          = error "It's empty."
  tail (Cons _ xs)  = xs

suffixes :: (Stack s) => s a -> s (s a)
suffixes = go
  where
    go xs
      | isEmpty xs  = cons xs empty
      | otherwise   = cons xs (go $ tail xs)
