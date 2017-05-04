{-# LANGUAGE InstanceSigs #-}

module Ch3.RedBlack where

import Ch2.Persistent.Tree (Set(..))

data Color = Red | Black

instance Show Color where
  show Red    = "R"
  show Black  = "B"

data Tree a = Empty | Tree Color (Tree a) a (Tree a)

instance (Show a) => Show (Tree a) where
  show Empty = "[]"
  show (Tree c l x r) = "[" ++ show x ++ "(" ++ show c ++ "), " ++
    show l ++ ", " ++ show r ++ "]"

balance:: Color -> Tree a -> a -> Tree a -> Tree a
balance Black (Tree Red (Tree Red a x b) y c) z d =
    Tree Red (Tree Black a x b) y (Tree Black c z d)
balance Black (Tree Red a x (Tree Red b y c)) z d =
    Tree Red (Tree Black a x b) y (Tree Black c z d)
balance Black a x (Tree Red (Tree Red b y c) z d) =
    Tree Red (Tree Black a x b) y (Tree Black c z d)
balance Black a x (Tree Red b y (Tree Red c z d)) =
    Tree Red (Tree Black a x b) y (Tree Black c z d)
balance color a x b = Tree color a x b

setOf:: (Ord a) => [a] -> Tree a
setOf = foldl (flip insert) empty

instance Set Tree where
  empty = Empty

  member:: (Ord a) => a -> Tree a -> Bool
  member _ Empty = False
  member x (Tree _ l y r)
    | x < y     = member x l
    | x > y     = member x r
    | otherwise = True

  insert x s = black $ ins s
    where
      ins Empty = Tree Red Empty x Empty
      ins t@(Tree c a y b)
        | x < y     = balance c (ins a) y b
        | x > y     = balance c a y $ ins b
        | otherwise = t
      black (Tree _ a y b) = Tree Black a y b
