{-# LANGUAGE RankNTypes #-}

module Ch5.Splay where

data Splay a = Empty | Tree (Splay a) a (Splay a)

instance (Show a) => Show (Splay a) where
  show Empty = "[]"
  show (Tree l x r) =
    "[" ++ show x ++ " l:" ++ show l ++ ", r:" ++ show r ++ "]"

type Partition a = (Ord a) => a -> Splay a -> Splay a

single:: (Ord a) => a -> Splay a
single x = Tree Empty x Empty

bigger:: Partition a
bigger _ Empty        = Empty
bigger p (Tree l x r)
  | x <= p    = bigger p r
  | otherwise = case l of
    Empty         -> Tree Empty x r
    Tree ll y lr  -> if y <= p
      then Tree (bigger p lr) x r
      else Tree (bigger p ll) y $ Tree lr x r

smaller:: Partition a
smaller _ Empty = Empty
smaller p (Tree l x r)
  | p < x       = smaller p l
  | otherwise   = case r of
    Empty         -> Tree l x Empty
    Tree rl y rr  ->
      if p < y
        then Tree l x $ smaller p rl
        else Tree (Tree l x rl) y $ smaller p rr
