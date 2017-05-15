module Ch6.Sortable where

class Sortable s where
  empty:: (Ord a) => s a

  add:: (Ord a) => a -> s a -> s a

  sorted:: (Ord a) => s a -> [a]
