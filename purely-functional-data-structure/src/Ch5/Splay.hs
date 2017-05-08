module Ch5.Splay where

data Splay a = Empty | Tree (Splay a) a (Splay a)

instance (Show a) => Show (Splay a) where
  show Empty = "[]"
  show (Tree l x r) =
    "[" ++ show x ++ " l:" ++ show l ++ ", r:" ++ show r ++ "]"

bigger:: (Ord a) => a -> Splay a -> Splay a
bigger _ Empty        = Empty
bigger p (Tree l x r)
  | x <= p    = bigger p r
  | otherwise = case l of
    Empty         -> Tree Empty x r
    Tree ll y lr  -> if y <= p
      then Tree (bigger p lr) x r
      else Tree (bigger p ll) y $ Tree lr x r


