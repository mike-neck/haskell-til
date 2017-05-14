module Ch6.PhysicistsQueue where

import Prelude hiding (head, tail)  
import Ch5.Queue

data PhysicistsQueue a = PhQueue ([a], [a]) (Int, Int)

instance Queue (PhysicistsQueue) where
  empty = PhQueue ([],[]) (0,0)
  isEmpty (PhQueue _ (l,_)) = l == 0

  offer x (PhQueue (h,t) (l,r)) = check $ PhQueue (h,x:t) (l,r + 1)

  head (PhQueue ([],_) (_,_))   = error "empty queue"
  head (PhQueue (x:_,_) (_,_))  = x

  tail (PhQueue ([],_) (_,_))   = error "empty queue"
  tail (PhQueue (x:hs,t) (l,r)) = check $ PhQueue (hs,t) (l - 1, r)

check:: PhysicistsQueue a -> PhysicistsQueue a
check q@(PhQueue (h,t) (l,r))
  | r <= l    = q
  | otherwise = PhQueue (h ++ reverse t, []) (l + r, 0)
