module DList where

type DList a = [a] -> [a]

fromList :: [a] -> DList a
fromList l = \x -> x Prelude.++ l

toList :: DList a -> [a]
toList d = d []

empty :: DList a
empty = \xs -> xs

singleton :: a -> DList a
singleton x = \ys -> x:ys

(++) :: DList a -> DList a -> DList a
(++) = (.)
