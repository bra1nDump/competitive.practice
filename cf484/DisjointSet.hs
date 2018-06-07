module DisjointSet (
  DisjointSet(..)
  , makeSet
  , find
  , union) where

data Element a = Element { id :: Int
                         , payload :: a
                         , parent :: Int}

data DisjointSet a = DisjointSet [()]
