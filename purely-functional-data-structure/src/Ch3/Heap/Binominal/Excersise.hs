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

  merge (BinHeap l) (BinHeap r) = BinHeap $ go l r
    where
      go:: (Ord a) => [(Int, Tree a)] -> [(Int, Tree a)] -> [(Int, Tree a)]
      go l [] = l
      go [] r = r
      go l@(x:xs) r@(y:ys)
        | rx < ry       = x : go xs r
        | rx > ry       = y : go l ys
        | otherwise     = insTreeRaw (rx, linked) $ go xs ys
          where
            rx = fst x
            ry = fst y
            linked = link x' y'
            x' = snd x
            y' = snd y

  findMin (BinHeap ts)      = go ts
    where
      go []         = Nothing
      go [(_,x)]    = Just $ root x
      go ((_,x):xs) = do
        y <- go xs
        if x' <= y
          then Just x'
          else Just y
          where
            x' = root x

  deleteMin (BinHeap ts) = fmap BinHeap <$> go ts
    where
      go:: (Ord a) => [(Int, Tree a)] -> Maybe (a, [(Int, Tree a)])
      go []             = Nothing
      go [(_,x)]        = Just (root x, [])
      go ((_,x):ts)  = do
        (y, ys) <- go ts
        if x' <= y
        then Just (x', ts)
        else Just (y, ys)
        where
          x' = root x
