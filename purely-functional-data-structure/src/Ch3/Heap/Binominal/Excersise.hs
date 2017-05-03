{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Ch3.Heap.Binominal.Excersise where

import Ch3.Heap

data Tree a = Node { root:: a, list:: [Tree a] }

newtype BinHeap a = BinHeap { elems:: [(Int, Tree a)] }

insTree:: (Ord a) => (Int, Tree a) -> BinHeap a -> BinHeap a
insTree t = BinHeap . (insTreeRaw t) . elems

insTreeRaw:: (Ord a) => (Int, Tree a) -> [(Int, Tree a)] -> [(Int, Tree a)]
insTreeRaw t@(rl, x) ts@((rr, y):ys)
  | rl < rr   = t:ts
  | otherwise = insTreeRaw (rl + 1, link x y) ys

link:: (Ord a) => Tree a -> Tree a -> Tree a
link x y
  | x `rootLeq` y = Node (root x) (y:(list x))
  | otherwise     = Node (root y) (x:(list y))

rootLeq:: (Ord a) => Tree a -> Tree a -> Bool
rootLeq x y = root x <= root y

instance Heap BinHeap where
  empty = BinHeap []
  isEmpty = null . elems

  insert x xs = insTree (0, Node x []) xs
