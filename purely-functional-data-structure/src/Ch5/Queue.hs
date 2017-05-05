module Ch5.Queue where

import Prelude hiding (head, tail)

class Queue q where
  empty:: q a
  isEmpty:: q a -> Bool

  offer:: a -> q a -> q a
  poll:: q a -> (a, q a)
  poll q = (head q, tail q)

  head:: q a -> a
  tail:: q a -> q a
