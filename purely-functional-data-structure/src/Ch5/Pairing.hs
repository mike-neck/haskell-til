module Ch5.Pairing where

import Ch3.Heap

data Pairing a = Empty | Tree a [Pairing a]

mergePairs::(Ord a) => [Pairing a] -> Pairing a
mergePairs []   = Empty
mergePairs [h]  = h
mergePairs (x:y:hs) = merge i $ mergePairs hs
  where
    i = merge x y

instance Heap Pairing where
  empty = Empty
  isEmpty Empty = True
  isEmpty _     = False

  findMin Empty       = Nothing
  findMin (Tree x _)  = Just x

  merge h Empty = h
  merge Empty h = h
  merge l@(Tree x ls) r@(Tree y rs)
    | x <= y    = Tree x (r:ls)
    | otherwise = Tree y (l:rs)

  insert x h  = merge (Tree x []) h

  deleteMin Empty       = Nothing
  deleteMin (Tree x h)  = Just (x, mergePairs h)
