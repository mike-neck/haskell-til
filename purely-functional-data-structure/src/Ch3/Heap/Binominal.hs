{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Ch3.Heap.Binominal where

import Ch3.Heap

data BinominalHeap a = Node Int a [BinominalHeap a]
type Bh a = BinominalHeap a
newtype BinHeap a = BinHeap { heap:: [BinominalHeap a] }
  deriving Show

instance (Show a) => Show (Bh a) where
  show (Node r x xs) =
    "<" ++ show x ++ "(" ++ show r ++ ") " ++ show xs ++ ">"

link:: (Ord a) => Bh a -> Bh a -> Bh a
link l@(Node lr lx lxs) r@(Node _ rx rxs)
  | lx <= rx  = Node (lr + 1) lx (r:lxs)
  | otherwise = Node (lr + 1) rx (l:rxs)

rank:: Bh a -> Int
rank (Node r _ _) = r

root:: Bh a -> a
root (Node _ x _) = x

insTree:: (Ord a) => Bh a -> BinHeap a -> BinHeap a
insTree x (BinHeap xs) = BinHeap $ insTreeRaw x xs

insTreeRaw:: (Ord a) => Bh a -> [Bh a] -> [Bh a]
insTreeRaw x [] = [x]
insTreeRaw x xs@(t:ts)
  | rank x < rank t = x:xs
  | otherwise       = insTreeRaw (link x t) ts

removeMinTree:: (Ord a) => [Bh a] -> Maybe (Bh a, [Bh a])
removeMinTree [] = Nothing
removeMinTree [x] = Just (x, [])
removeMinTree (x:xs) = do
  (y, ys) <- removeMinTree xs
  if root x <= root y
    then Just (x, xs)
    else Just (y, x:ys)

mapFst:: (a -> b) -> (a, c) -> (b, c)
mapFst f (x, y) = (f x, y)

instance Heap BinHeap where
  empty = BinHeap []
  isEmpty = null . heap

  insert:: (Ord a) => a -> BinHeap a -> BinHeap a
  insert x xs = flip insTree xs $ Node 0 x []

  merge:: (Ord a) => BinHeap a -> BinHeap a -> BinHeap a
  merge (BinHeap l) (BinHeap r) = BinHeap $ go l r
    where
      go l [] = l
      go [] r = r
      go l@(x:xs) r@(y:ys)
        | rank x < rank y = x : go xs r
        | rank x > rank y = y : go l ys
        | otherwise       = insTreeRaw linked $ go xs ys
          where
            linked = link x y

  findMin:: (Ord a) => BinHeap a -> Maybe a
  findMin (BinHeap xs) = go xs
    where
      go = (fmap firstRoot) <$> removeMinTree
      firstRoot = root . fst

  deleteMin:: (Ord a) => BinHeap a -> Maybe (a, BinHeap a)
  deleteMin (BinHeap xs) = fmap BinHeap <$> go xs
    where
      go:: (Ord a) =>  [Bh a] -> Maybe (a, [Bh a])
      go = fmap firstRoot <$> removeMinTree
      firstRoot = mapFst root
