module Array where

import Data.Array
    ( Ix(range), Array, listArray, bounds, array, (!), ixmap )

tabulate :: Ix i => (i, i) -> (i -> a) -> Array i a
tabulate (u, v) f = array (u, v) [ (i, f i) | i <- range (u, v)]

arrayTake :: Ix i => Array i a -> (i, i) -> [a]
arrayTake arr (u, v) = [arr ! i | i <- range (u, v)]

arrayMultiply :: Array Int a -> Int -> Array Int a
arrayMultiply arr n = listArray (0, n * l - 1) [arr ! (i `mod` l) | i <- [0.. n * l - 1]]
  where
    l = length arr

transpose :: Ix i => Array (i, i) a -> Array (i, i) a
transpose m = ixmap (swap l, swap u) swap  m
  where
    swap = \(r, c) -> (c, r)
    (l, u) = bounds m