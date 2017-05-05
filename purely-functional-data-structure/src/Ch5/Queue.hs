module Ch5.Queue where

import Prelude hiding (head, tail)

class Queue q where
  empty:: q a
  isEmpty:: q a -> Bool

  enqueue:: a -> q a -> q a
  dequeue:: q a -> (a, q a)
  dequeue q = (head q, tail q)

  head:: q a -> a
  tail:: q a -> q a

newtype DefaultQueue a = DefaultQueue { getQueue:: ([a], [a]) }

instance Queue DefaultQueue where
  empty = DefaultQueue ([], [])
  isEmpty (DefaultQueue (f, _)) = null f

  enqueue x (DefaultQueue ([], _))  = DefaultQueue ([x], [])
  enqueue x (DefaultQueue (f, r))   = DefaultQueue (f, x:r)

  head (DefaultQueue ([], _))   = error "It's empty."
  head (DefaultQueue (x:f, r))  = x

  tail (DefaultQueue ([], _))   = error "It's empty."
  tail (DefaultQueue ([_], r))  = DefaultQueue (reverse r, [])
  tail (DefaultQueue (_:f, r))  = DefaultQueue $ checkf (f, r)
    where
      checkf ([], r)  = (reverse r, [])
      checkf t        = t
