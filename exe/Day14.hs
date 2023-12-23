module Main where

-- Base
import Data.Array (Array, bounds, (!))
import qualified Data.Map as M
import Control.Arrow ((>>>))

-- Mine
import Common (adventOfCode, arrayParse)

type Grid = Array (Int, Int) Char
data Rock = Cube {loc :: (Int, Int)} | Round {loc :: (Int, Int)} deriving (Show, Eq)

-- Just compare based on position
instance Ord Rock where
  r <= r' = loc r <= loc r'

getRocks :: Grid -> [[Rock]]
getRocks grid = map (getRocksOneCol grid) [1 .. w]
  where (_, (h, w)) = bounds grid

getRocksOneCol :: Grid -> Int -> [Rock]
getRocksOneCol arr c = Cube (0, c) : foldr updateRocks [Cube (h + 1, c)] [1 .. h]
  where 
    (_, (h, w)) = bounds arr
    updateRocks :: Int -> [Rock] -> [Rock]
    updateRocks r rocks
      | arr ! (r, c) == '#' = Cube (r, c) : rocks
      | arr ! (r, c) == 'O' = Round (r, c) : rocks
      | otherwise           = rocks

convertToHorizontal :: (Int, Int) -> Int -> [[Rock]] -> [[Rock]]
convertToHorizontal (h, w) 0 vertRocks = convertToHorizontal (h, w) 1 (map (init . tail) vertRocks) -- get rid of all the sentinels
convertToHorizontal (h, w) i vertRocks
  | h + 1 == i = []
  | otherwise = nextRow : convertToHorizontal (h, w) (i + 1) remaining
  where 
    nextRow = Cube (i, 0) : [head col | col <- vertRocks, not . null $ col, let (r, c) = loc (head col), r == i] ++ [Cube (i, w + 1)]
    remaining = [if r == i then tail col else col | col <- vertRocks, not . null $ col, let (r, c) = loc (head col)] 

convertToVertical :: (Int, Int) -> Int -> [[Rock]] -> [[Rock]]
convertToVertical (h, w) 0 horizRocks = convertToVertical (h, w) 1 (map (init . tail) horizRocks) -- get rid of all the sentinels
convertToVertical (h, w) i horizRocks
  | w + 1 == i = []
  | otherwise = nextCol : convertToVertical (h, w) (i + 1) remaining
  where 
    nextCol = Cube (0, i) : [head row | row <- horizRocks, not . null $ row, let (r, c) = loc (head row), c == i] ++ [Cube (h + 1, i)]
    remaining = [if c == i then tail row else row | row <- horizRocks, not . null $ row, let (r, c) = loc (head row)] 

tipRocks :: ((Int, Int) -> (Int, Int)) -> [[Rock]] -> [[Rock]]
tipRocks f rocks = map (tipRocksOneCol f) rocks

tipRocksOneCol :: ((Int, Int) -> (Int, Int)) -> [Rock] -> [Rock]
tipRocksOneCol f [] = []
tipRocksOneCol f [r] = [r]
tipRocksOneCol f (x : (Round _) : ys) = x : tipRocksOneCol f (Round (f (loc x)):ys) 
tipRocksOneCol f (x : (Cube r) : ys)  = x : tipRocksOneCol f ((Cube r):ys)

scoreRocks ::  ((Int, Int) -> Int) -> Int -> [[Rock]] -> Int
scoreRocks f h = sum . map (scoreRocksOneCol f h) 

scoreRocksOneCol :: ((Int, Int) -> Int) -> Int -> [Rock] -> Int
scoreRocksOneCol f _ []             = 0
scoreRocksOneCol f h (Cube _ : rs)  = scoreRocksOneCol f h rs
scoreRocksOneCol f h (Round r : rs) = scoreRocksOneCol f h rs + (h - (f r) + 1)

tipNorth = tipRocks (\(r, c) -> (r + 1, c))
tipWest = tipRocks (\(r, c) -> (r, c + 1))
tipSouth = tipRocks (\(r, c) -> (r - 1, c))
tipEast = tipRocks (\(r, c) -> (r, c - 1))

day14a :: Grid -> Int
day14a arr = (scoreRocks fst h) . tipNorth . getRocks $ arr
  where (_, (h, w)) = bounds arr

spinCycle :: (Int, Int) -> [[Rock]] -> [[Rock]]
spinCycle (h, w) = tipNorth >>>
   (convertToHorizontal (h, w) 0) >>> tipWest >>>
   (convertToVertical (h, w) 0) >>> (map reverse) >>> tipSouth >>> (map reverse) >>>
   (convertToHorizontal (h, w) 0) >>> (map reverse) >>> tipEast >>> (map reverse) >>>
   (convertToVertical (h, w) 0)

cacheSpins :: (Int, Int) -> Int -> [[Rock]] -> M.Map [[Rock]] Int -> (Int, Int, [[[Rock]]])
cacheSpins (h, w) i rs m
  | rs `M.member` m = (i, m M.! rs, [])
  | otherwise       = (cycleEnd, cycleStart, rs:rss)
  where (cycleEnd, cycleStart, rss) = (cacheSpins (h, w) (i + 1) (spinCycle (h, w) rs) (M.insert rs i m))

day14b :: Grid -> Int
day14b arr = scoreRocks fst h (rss !! index)
  where
    (_, (h, w)) = bounds arr
    (cycleEnd, cycleStart, rss) = cacheSpins (h, w) 0 (getRocks arr) (M.empty)
    cycleLen = cycleEnd - cycleStart
    index = cycleStart + (1000000000 - cycleEnd) `mod`cycleLen

inp = "O....#....\nO.OO#....#\n.....##...\nOO.#O....O\n.O.....O#.\nO.#..O.#.#\n..O..#O..O\n.......O..\n#....###..\n#OO..#...."

main :: IO ()
main = adventOfCode 14 arrayParse day14a day14b
