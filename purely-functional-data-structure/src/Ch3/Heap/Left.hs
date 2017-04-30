module Ch3.Heap.Left where

import Ch3.Heap

data LeftHeap a = Empty | Tree Int a (LeftHeap a) (LeftHeap a)

instance (Show a) => Show (LeftHeap a) where
  show Empty = "[]"
  show (Tree s x l r) =
    "[" ++ show x ++ "(" ++ show s ++ "), " ++ show l ++ ", " ++ show r ++ "]"

single:: a -> LeftHeap a
single x = Tree 1 x Empty Empty

rank:: LeftHeap a -> Int
rank Empty = 0
rank (Tree r _ _ _) = r

increment:: LeftHeap a -> Int
increment = (+ 1) . rank

instance Heap LeftHeap where
  empty = Empty
  isEmpty = (== 0) . rank

  merge Empty t = t
  merge t Empty = t
  merge hl@(Tree dl xl ll rl) hr@(Tree dr xr lr rr)
    | xl <= xr  = makeTree xl ll $ merge rl hr
    | otherwise = makeTree xr lr $ merge hl rr

  insert x h = flip merge h $ Tree 1 x Empty Empty

  findMin Empty           = Nothing
  findMin (Tree _ x _ _)  = Just x

  deleteMin Empty           = Nothing
  deleteMin (Tree _ x l r)  = Just (x, merge l r)

makeTree:: (Ord a) => a -> LeftHeap a -> LeftHeap a -> LeftHeap a
makeTree x l r
  | rank l >= rank r  = Tree (increment r) x l r
  | otherwise         = Tree (increment l) x r l

makeLeftHeap:: (Ord a) => [a] -> LeftHeap a
makeLeftHeap = flip foldl Empty $ flip insert

--impMakeLeftHeap:: (Ord a) => [a] -> LeftHeap a
--impMakeLeftHeap = go [] $ fmap single
--  where
--    go:: (Ord a) => [LeftHeap a] -> [LeftHeap a] -> LeftHeap a
--    go [] [] = Empty
--    go [] (x:xs) = go []
--    go (x:xs) = go' x xs
--
--    go':: (Ord a) => LeftHeap a -> [LeftHeap a] -> [LeftHeap a] -> LeftHeap a
--    go' x [] = x
