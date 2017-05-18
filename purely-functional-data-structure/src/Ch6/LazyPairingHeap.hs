module Ch6.LazyPairingHeap where

import Ch3.Heap(Heap(..))

data PairHeap a = Empty | Tree a (PairHeap a) (PairHeap a)

link:: (Ord a) => PairHeap a -> PairHeap a -> PairHeap a
link (Tree x Empty t) r = Tree x r t
link (Tree x h t) r     = Tree x Empty $ flip merge t $ merge r h

instance Heap (PairHeap) where
  empty = Empty

  isEmpty Empty = True
  isEmpty _     = False

  insert x Empty = Tree x Empty Empty

  merge lh Empty = lh
  merge Empty rh = rh
  merge l@(Tree x _ _) r@(Tree y _ _)
    | x <= y    = link l r
    | otherwise = link r l

  findMin Empty         = Nothing
  findMin (Tree x _ _)  = Just x

  deleteMin Empty         = Nothing
  deleteMin (Tree x h t)  = Just $ (x, merge h t)
