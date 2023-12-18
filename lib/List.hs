module List where

import Data.Ord (comparing)
import Data.List (maximumBy, group, sort)

replace :: (Eq a) => a -> a -> [a] -> [a]
replace _ _ [] = []
replace s t (x:xs)
  | s == x    = t:replace s t xs
  | otherwise = x:replace s t xs

mostCommon :: (Ord a) => [a] -> a
mostCommon xs = fst (maximumBy (comparing snd) [(c, length g) | g@(c:_) <- group (sort xs)])
