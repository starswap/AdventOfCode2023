module Main where

-- Base
import Data.Array (Array, (!), assocs, bounds)
import qualified Data.Set as S

-- Mine
import Common (adventOfCode, arrayParse)

type Coords = (Int, Int)
type Map = Array Coords Char

ds :: [Coords]
ds = [(-1, 0), (0, -1), (0, 1), (1, 0)]

adj :: Int -> Int -> Coords -> [Coords]
adj w h (sr, sc) = [(sr + dr, sc + dc) | (dr, dc) <- ds, sr + dr >= 1, sr + dr <= h, sc + dc >= 1, sc + dc <= w]

next :: Map -> Coords -> Coords -> Coords
next m (r, c) now@(r', c') =
  case m ! now of
    '|' -> (r' + dr, c')
    '-' -> (r', c' + dc)
    'L' -> (r' + dc, c' + dr)
    'J' -> (r' - dc, c' - dr)
    '7' -> (r' + dc, c' + dr)
    'F' -> (r' - dc, c' - dr)
    _   -> error "Invalid symbol in grid!"
  where dr = r' - r
        dc = c' - c

hasLeftPipe :: Char -> Bool
hasLeftPipe ch = ch == 'J' || ch == '7' || ch == '-'

hasRightPipe :: Char -> Bool
hasRightPipe ch = ch == 'F' || ch == 'L' || ch == '-'

hasTopPipe :: Char -> Bool
hasTopPipe ch = ch == 'L' || ch == 'J' || ch == '|'

hasBottomPipe :: Char -> Bool
hasBottomPipe ch = ch == 'F' || ch == '7' || ch == '|'

isConn :: Map -> Coords -> Coords -> Bool
isConn m (r, c) n@(r', c') =
  (dr == 0 && dc ==  1 && hasLeftPipe ch) ||
  (dr == 0 && dc == -1 && hasRightPipe ch) ||
  (dr == 1 && dc ==  0 && hasTopPipe ch) ||
  (dr == -1 && dc == 0 && hasBottomPipe ch)
  where dr = r' - r
        dc = c' - c
        ch = m ! n

findStart :: Map -> Coords
findStart m = head [i | (i, e) <- assocs m, e == 'S']

findFirstTwo :: Map -> (Coords, Coords)
findFirstTwo m = (start, second)
  where
    (_, (h, w)) = bounds m
    start = findStart m
    second = head . filter (isConn m start) $ (adj w h start)

pathLength :: Map -> Coords -> Coords -> Int
pathLength m prev now
  | m ! now == 'S' = 1
  | otherwise      = 1 + pathLength m now (next m prev now)

day10a :: Map -> Int
day10a m = div (pathLength m start second) 2
  where (start, second) = findFirstTwo m

augNode :: Coords -> Coords
augNode (r, c) = (2 * r, 2 * c)

inOriginalGrid :: Coords -> Bool
inOriginalGrid (r, c) = even r && even c

augGridPathNodes :: Map -> Coords -> Coords -> [Coords]
augGridPathNodes m prev@(pr, pc) now@(nr, nc)
  | m ! now == 'S' = mid:an:[]
  | otherwise      = mid:an:augGridPathNodes m now (next m prev now)
  where
    an@(ar, ac) = augNode now
    (dr, dc)    = (nr - pr, nc - pc)
    mid         = (ar - dr, ac - dc)

floodFill :: S.Set Coords -> S.Set Coords -> Int -> Int -> Coords -> S.Set Coords
floodFill pipes visited w h n
  | n `S.member` pipes || n `S.member` visited = visited
  | otherwise                                  = recursive
  where
    recursive = foldr (\c vis -> floodFill pipes vis w h c) (S.insert n visited) (adj w h n)

day10b :: Map -> Int
day10b m = answer
  where
    (_, (h, w))     = bounds m
    augW            = 2 * w + 1
    augH            = 2 * h + 1
    (start, second) = findFirstTwo m
    path            = S.fromList (augGridPathNodes m start second)
    wholeGrid       = S.fromList ([(r, c) | r <- [1 .. augH], c <- [1 .. augW]])
    outside         = floodFill path S.empty augW augH (1, 1)
    inside          = (wholeGrid S.\\ outside) S.\\ path
    answer          = length (S.filter inOriginalGrid inside)

main :: IO ()
main = adventOfCode 10 (day10a . arrayParse) (day10b . arrayParse)
