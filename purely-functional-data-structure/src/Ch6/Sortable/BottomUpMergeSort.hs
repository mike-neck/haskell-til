module Ch6.Sortable.BottomUpMergeSort where

import Ch6.Sortable

data Sort a = Sort {
  size:: Int
, list:: [[a]]
}

merge:: (Ord a) => [a] -> [a] -> [a]
merge [] ys                 = ys
merge xs []                 = xs
merge xs@(x:xs') ys@(y:ys')
  | x <= y    = x : merge xs' ys
  | otherwise = y : merge xs ys'

instance Sortable Sort where
  empty = Sort 0 []

  add x (Sort size segs) = Sort (size + 1) $ addSeg [x] segs size
    where
      addSeg:: (Ord a) => [a] -> [[a]] -> Int -> [[a]]
      addSeg s ss sz
        | sz `mod` 2 == 0 = s : ss
        | otherwise       =
          addSeg (merge s $ head ss) (tail ss) (sz `div` 2)

  sorted (Sort _ ts) = foldl merge [] ts
