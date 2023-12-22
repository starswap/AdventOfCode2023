module Main where

-- Base
import Data.Array (Array, (!), bounds, ixmap)

-- Mine
import Common (adventOfCode, arrayParse)

type Coords = (Int, Int)
type Map = Array Coords Char

findGalaxies :: Map -> Coords -> [Coords]
findGalaxies m p@(r, c)
  | r == h && c == w + 1    = []
  | c == w + 1 || ch /= '#' = findGalaxies m next
  | otherwise               = p:findGalaxies m next
  where
    (_, (h, w)) = bounds m
    ch = m ! p
    next = if c == w + 1 then (r + 1, 1) else (r, c + 1)

emptyRows :: Map -> Coords -> Bool -> [Int]
emptyRows m p@(r, c) b
  | c == w + 1 && b         = r : emptyRows m next True
  | c == w + 1 && not b     = emptyRows m next True
  | r == h + 1 && c == 1    = []
  | otherwise               = emptyRows m next (b && ch == '.')
  where
    (_, (h, w)) = bounds m
    ch = m ! p
    next = if c == w + 1 then (r + 1, 1) else (r, c + 1)

countBetween :: Ord a => a -> a -> [a] -> Int
countBetween l u = length . takeWhile (<= u) . dropWhile (< l)

dist :: Integer -> Coords -> Coords -> [Int] -> [Int] -> Integer
dist d (r, c) (r', c') emR emC = xDist + yDist
  where
    xDist = toInteger (abs (c - c')) + d * toInteger (countBetween (c `min` c') (c `max` c') emC)
    yDist = toInteger (abs (r - r')) + d * toInteger (countBetween (r `min` r') (r `max` r') emR)

day11 :: Integer -> Map -> Integer
day11 d m = sum [dist d g g' emR emC | g <- gals, g' <- gals, g < g']
  where
    gals = findGalaxies m (1, 1)
    emR = emptyRows m (1, 1) True
    emC = emptyRows (ixmap (bounds m) (\(r, c) -> (c, r)) m) (1, 1) True

main :: IO ()
main = adventOfCode 11 arrayParse (day11 1) (day11 999999)
