module Ch5.Deque where

import Prelude hiding (head, tail, init, last)
import Ch5.Queue(Queue(..))

class (Queue d) => Deque d where
  push:: a -> d a -> d a
  pop:: d a -> (a, d a)
  pop q = (last q, init q)

  last:: d a -> a
  init:: d a -> d a

newtype DefaultQueue a = DefaultQueue { getQueue:: ([a], [a]) }

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
