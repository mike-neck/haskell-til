module Ch5.Queue.Impl(
  toQueue
)where

import Ch5.Queue.Interface

newtype DefaultQueue a = DefaultQueue { getQueue:: ([a], [a]) }

toQueue:: [a] -> DefaultQueue a
toQueue xs = DefaultQueue (xs, [])

instance (Show a) => Show (DefaultQueue a) where
  show (DefaultQueue (a,b)) = "Queue[" ++ go (a, reverse b)
    where
      go ([], []) = "]"
      go ([], x:xs) = "," ++ show x ++ go ([],xs)
      go ([x], ys)  = show x ++ go ([], ys)
      go (x:xs, ys) = show x ++ "," ++ go (xs, ys)

instance Queue DefaultQueue where
  empty = DefaultQueue ([], [])
  isEmpty (DefaultQueue (f, _)) = null f

  offer x (DefaultQueue ([], _))  = DefaultQueue ([x], [])
  offer x (DefaultQueue (f, r))   = DefaultQueue (f, x:r)

  head (DefaultQueue ([], _))   = error "It's empty."
  head (DefaultQueue (x:f, r))  = x

  tail (DefaultQueue ([], _))   = error "It's empty."
  tail (DefaultQueue ([_], r))  = DefaultQueue (reverse r, [])
  tail (DefaultQueue (_:f, r))  = DefaultQueue $ checkf (f, r)

checkf:: ([a], [a]) -> ([a],[a])
checkf ([], r)  = (reverse r, [])
checkf t        = t

instance Deque DefaultQueue where
  push x (DefaultQueue ([], _)) = DefaultQueue ([x], [])
  push x (DefaultQueue (f, r)) = DefaultQueue (x:f, r)

  last (DefaultQueue ([], _))     = error "It's empty"
  last (DefaultQueue (f, []))     = let (x:xs) = reverse f in x
  last (DefaultQueue (_, (x:xs))) = x

  init (DefaultQueue ([], _))     = error "It's empty"
  init (DefaultQueue (f, []))     = let (x:xs) = reverse f in
    DefaultQueue $ checkf ([], xs)
  init (DefaultQueue (f, (x:xs))) = DefaultQueue (f, xs)


