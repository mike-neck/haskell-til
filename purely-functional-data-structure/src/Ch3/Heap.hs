module Ch3.Heap where

import Data.Maybe(fromMaybe)

class Heap h where
  empty:: (Ord a) => h a
  isEmpty:: (Ord a) => h a -> Bool

  insert:: (Ord a) => a -> h a -> h a
  merge:: (Ord a) => h a -> h a -> h a

  findMin:: (Ord a) => h a -> Maybe a
  deleteMin:: (Ord a) => h a -> Maybe (a, h a)

toHeap:: (Ord a, Heap h) => [a] -> h a
toHeap = foldl (flip insert) empty

data ExplicitMin h a = Vac | Oqp a (h a)

instance (Heap h) => Heap (ExplicitMin h) where
  empty = Vac
  isEmpty Vac = True
  isEmpty _   = False

  insert x Vac        = Oqp x $ insert x (insert x empty)
  insert x (Oqp y h)
    | x < y     = Oqp x heap
    | otherwise = Oqp y heap
      where
        heap = insert x h

  merge Vac e = e
  merge (Oqp x l) (Oqp y r)
    | x < y     = Oqp x heap
    | otherwise = Oqp y heap
    where
      heap = merge l r

  findMin Vac       = Nothing
  findMin (Oqp x _) = Just x

  deleteMin Vac       = Nothing
  deleteMin (Oqp x h) = Just (x, heap)
    where
      heap = fromMaybe Vac del
      del = do
        (_, h') <- deleteMin h
        m <- findMin h'
        return $ Oqp m h'
