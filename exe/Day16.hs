module Main where

-- Base
import qualified Data.Map as M
import Data.Array (Array, bounds, (!))

-- Mine
import Common (adventOfCode, arrayParse)
import Array (inBounds)
import Tuple ((+++))

type Coords = (Int, Int)
type Grid = Array Coords Char
type Direction = (Int, Int)

left :: Direction
left = (0, -1)

right :: Direction
right = (0, 1)

up :: Direction
up = (-1, 0)

down :: Direction
down = (1, 0)

updateSeen :: Coords -> Direction -> M.Map Coords [Direction] -> M.Map Coords [Direction]
updateSeen x dir = M.insertWith (++) x [dir] 

applyEmptySpace :: Grid -> Coords -> Direction -> M.Map Coords [Direction] -> M.Map Coords [Direction]
applyEmptySpace g c d seen = traverseGrid g (c +++ d) d seen

applyVerticalSplitter :: Grid -> Coords -> Direction -> M.Map Coords [Direction] -> M.Map Coords [Direction]
applyVerticalSplitter g c (0, _) seen = applyEmptySpace g c (-1, 0) (applyEmptySpace g c (1, 0) seen)
applyVerticalSplitter g c d seen      = applyEmptySpace g c d seen

applyHorizontalSplitter :: Grid -> Coords -> Direction -> M.Map Coords [Direction] -> M.Map Coords [Direction]
applyHorizontalSplitter g c (_, 0) seen = applyEmptySpace g c (0, -1) (applyEmptySpace g c (0, 1) seen)
applyHorizontalSplitter g c d seen      = applyEmptySpace g c d seen

applyRightMirror :: Grid -> Coords -> Direction -> M.Map Coords [Direction] -> M.Map Coords [Direction]
applyRightMirror g co (r, c) seen = applyEmptySpace g co (negate c, negate r) seen

applyLeftMirror :: Grid -> Coords -> Direction -> M.Map Coords [Direction] -> M.Map Coords [Direction]
applyLeftMirror g co (r, c) seen = applyEmptySpace g co (c, r) seen

traverseGrid :: Grid -> Coords -> Direction -> M.Map Coords [Direction] -> M.Map Coords [Direction]
traverseGrid g x@(r, c) d seen
  | not . inBounds g $ (r, c)  = seen
  | seenBefore                 = seen
  | g ! (r, c) == '.'          = applyEmptySpace g x d seen'
  | g ! (r, c) == '|'          = applyVerticalSplitter g x d seen'
  | g ! (r, c) == '-'          = applyHorizontalSplitter g x d seen'
  | g ! (r, c) == '/'          = applyRightMirror g x d seen'
  | g ! (r, c) == '\\'         = applyLeftMirror g x d seen'
  | otherwise                  = error "Unrecognised symbol in grid"
  where
    seen' = updateSeen x d seen
    seenBefore = d `elem` (M.findWithDefault [] (r, c) seen)

tilesEnergised :: Grid -> Coords -> Direction -> Int
tilesEnergised g c d = length (traverseGrid g c d M.empty)

day16a :: Grid -> Int
day16a g = tilesEnergised g (1, 1) (0, 1)

day16b :: Grid  -> Int
day16b g = maximum [d, u, r, l]
  where
    (_, (w, h)) = bounds g
    d = maximum [tilesEnergised g (1, c) down | c <- [1 .. w]]
    u = maximum [tilesEnergised g (h, c) up | c <- [1 .. w]]
    r = maximum [tilesEnergised g (r, 1) right | r <- [1 .. h]]
    l = maximum [tilesEnergised g (r, w) left | r <- [1 .. h]]

main :: IO ()
main = adventOfCode 16 arrayParse day16a day16b
