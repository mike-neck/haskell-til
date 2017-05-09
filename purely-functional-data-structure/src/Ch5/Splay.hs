{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}

module Ch5.Splay where

data Splay a = Empty | Tree (Splay a) a (Splay a)
  deriving Functor

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

partition:: (Ord a) => a -> Splay a -> (Splay a, Splay a)
partition _ Empty = (Empty, Empty)
partition p t@(Tree l x r)
  | x <= p    = case r of
    Empty         -> (t, Empty)
    Tree rl y rr  -> if y <= p
      then let
        (s,b) = partition p rr
        tl = Tree l x rl
        in (Tree tl y s, b)
      else let
        (s,b) = partition p rl
        in (Tree l x s, Tree b y rr)
  | otherwise = case l of
    Empty         -> (Empty, t)
    Tree ll y lr  -> if y <= p
      then let
        (s,b) = partition p lr
          in (Tree ll y s, Tree b x r)
      else let
        (s,b) = partition p ll
        tr = Tree lr x r
          in (s, Tree b y tr)
