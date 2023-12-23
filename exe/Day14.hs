module Main where

-- Base
import Data.Array (Array, bounds, (!))

-- Mine
import Common (adventOfCode, arrayParse)

type Grid = Array (Int, Int) Char
data Rock = Cube {loc :: Int} | Round {loc :: Int} deriving Show

getRocks :: Grid -> Int -> [Rock]
getRocks arr c = Cube 0 : foldr updateRocks [] [1 .. h]
  where 
    (_, (h, w)) = bounds arr
    updateRocks :: Int -> [Rock] -> [Rock]
    updateRocks r rocks
      | arr ! (r, c) == '#' = Cube r : rocks
      | arr ! (r, c) == 'O' = Round r : rocks
      | otherwise           = rocks

compressRocks :: [Rock] -> [Rock]
compressRocks [] = []
compressRocks [r] = [r]
compressRocks (x : (Round _) : ys) = x : compressRocks (Round (loc x + 1):ys) 
compressRocks (x : (Cube r) : ys)  = x : compressRocks ((Cube r):ys)

scoreRocks :: Int -> [Rock] -> Int
scoreRocks _ []             = 0
scoreRocks h (Cube _ : rs)  = scoreRocks h rs
scoreRocks h (Round r : rs) = scoreRocks h rs + (h - r + 1) 

day14a :: Grid -> Int
day14a arr = sum . map ((scoreRocks h) . compressRocks . (getRocks arr)) $ [1 .. w]
  where (_, (h, w)) = bounds arr

day14b :: Grid -> Int
day14b = undefined

main :: IO ()
main = adventOfCode 14 arrayParse day14a day14b
