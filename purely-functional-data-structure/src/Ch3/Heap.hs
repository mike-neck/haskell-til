module Ch3.Heap where

class Heap h where
  rank:: h a -> Int

  empty:: (Ord a) => h a
  isEmpty:: (Ord a) => h a -> Bool

  insert:: (Ord a) => a -> h a -> h a
  merge:: (Ord a) => h a -> h a -> h a

  findMin:: (Ord a) => h a -> Maybe a
  deleteMin:: (Ord a) => h a -> Maybe (a, h a)
