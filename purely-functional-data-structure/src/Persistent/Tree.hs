{-# LANGUAGE InstanceSigs #-}

module Persistent.Tree where

class Set s where
  empty :: (Ord a) => s a
  insert:: (Ord a) => a -> s a -> s a
  member:: (Ord a) => a -> s a -> Bool

data Tree a = Empty | Tree (Tree a) a (Tree a)
  deriving Show

instance Set Tree where
  empty:: (Ord a) => Tree a
  empty = Empty

  member:: (Ord a) => a -> Tree a -> Bool
  member _ Empty        = False
  member x (Tree l y r)
    | x < y     = member x l
    | x > y     = member x r
    | otherwise = True

  insert:: (Ord a) => a -> Tree a -> Tree a
  insert x Empty        = Tree Empty x Empty
  insert x t@(Tree l y r)
    | x < y     = Tree (insert x l) y r
    | x > y     = Tree l y $ insert x r
    | otherwise = t

treeOf:: (Ord a) => [a] -> Tree a
treeOf xs = foldl (flip insert) Empty xs

impMember:: (Ord a) => a -> Tree a -> Bool
impMember _ Empty   = False
impMember x (Tree l y r)
  | x < y     = impMember x l
  | otherwise = go x r y
  where
    go x Empty c = x == c
    go x (Tree l y r) c
      | x < y     = go x l c
      | otherwise = go x r y
