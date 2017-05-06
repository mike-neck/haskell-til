module Ch5.Queue.Interface where

import Prelude hiding (head, tail, init, last)

class Queue q where
  empty:: q a
  isEmpty:: q a -> Bool

  offer:: a -> q a -> q a
  poll:: q a -> (a, q a)
  poll q = (head q, tail q)

  head:: q a -> a
  tail:: q a -> q a

class (Queue d) => Deque d where
  push:: a -> d a -> d a
  pop:: d a -> (a, d a)
  pop q = (last q, init q)

  last:: d a -> a
  init:: d a -> d a
