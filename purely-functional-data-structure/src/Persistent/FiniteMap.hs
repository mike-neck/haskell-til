{-# LANGUAGE MultiParamTypeClasses #-}

module Persistent.FiniteMap where

class FiniteMap fm where
  empty:: (Ord k) => fm k a
  bind:: (Ord k) => k -> a -> fm k a -> fm k a
  lookup:: (Ord k) => k -> fm k a -> a

data FM k a = Empty | Tree (FM k a) (k, a) (FM k a)

instance FiniteMap FM where
  empty = Empty

  lookup _ Empty  = error "It's empty."
  lookup x t      = go x t Nothing
    where
      go:: (Ord k) => k -> FM k a -> Maybe (k, a) -> a
      go x Empty Nothing = error "Not found."
      go x Empty (Just (k, v))
        | x == k    = v
        | otherwise = error "Not found."
      go x (Tree l e@(k, _) r) m
        | x < k     = go x l m
        | otherwise = go x r $ Just e

  bind k x Empty  = Tree Empty (k, x) Empty
  bind k x t      = undefined
