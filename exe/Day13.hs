module Main where

--Base
import Data.Array (Array, bounds, (!), indices)
import Data.List (groupBy, find)
import Data.Maybe (fromJust)

-- Mine
import Common (adventOfCode, Input, arrayParse)
import Array (transpose)

data TogglableArray = TogglableArray (Maybe (Int, Int)) (Array (Int, Int) Char)

bounds :: TogglableArray -> ((Int, Int), (Int, Int))
bounds (TogglableArray _ a) = Data.Array.bounds a

transpose :: TogglableArray -> TogglableArray
transpose (TogglableArray Nothing a) = TogglableArray Nothing (Array.transpose a)
transpose (TogglableArray (Just (r, c)) a) = TogglableArray (Just (c, r)) (Array.transpose a)

toggle :: Char -> Char
toggle '.' = '#'
toggle '#' = '.'

toggleArray :: TogglableArray -> (Int, Int) -> TogglableArray
toggleArray (TogglableArray _ a) ind = TogglableArray (Just ind) a

(!~) :: TogglableArray -> (Int, Int) -> Char
(TogglableArray Nothing arr) !~ i = arr ! i
(TogglableArray (Just togg) arr) !~ i 
  | i == togg = toggle (arr ! i)
  | otherwise = arr ! i

togglableArray :: Array (Int, Int) Char -> TogglableArray
togglableArray a = TogglableArray Nothing a

rowEquals :: TogglableArray -> Int -> Int -> Bool
rowEquals arr r1 r2 = and [arr !~ (r1, i) == arr !~ (r2, i) | i <- [1 .. w]]
  where (_, (h, w)) = Main.bounds arr

isReflection :: TogglableArray -> Int -> Bool
isReflection arr r = and (zipWith (rowEquals arr) [r, r - 1 .. 1] [r + 1 .. h])
  where (_, (h, w)) = Main.bounds arr 

findRowReflections :: TogglableArray -> [Int]
findRowReflections arr = filter (isReflection arr) [1 .. h - 1]
  where (_, (h, w)) = Main.bounds arr 

getListOfMatrixScores :: TogglableArray -> [Int]
getListOfMatrixScores a = map (100 *) rowRef ++ colRef
  where
    rowRef = findRowReflections a
    colRef = findRowReflections (Main.transpose a)

getMatrixScoreA :: TogglableArray -> Int
getMatrixScoreA = head . getListOfMatrixScores

getMatrixScoreB :: TogglableArray -> Int
getMatrixScoreB a@(TogglableArray _ arr)
  = head [
      fromJust newMirrorLine
      | toggleloc <- indices arr
      , let scoresList = getListOfMatrixScores (toggleArray a toggleloc)
      , let newMirrorLine = find (/= original) scoresList
      , newMirrorLine /= Nothing
    ]
  where
    original = getMatrixScoreA a

day13parse :: Input -> [TogglableArray] 
day13parse inp = [togglableArray . arrayParse . unlines $ g | g <- grps, not . null . head $ g]
  where grps = groupBy (\xs ys -> null xs == null ys) (lines inp)

day13 :: (TogglableArray -> Int) -> [TogglableArray] -> Int
day13 getMatrixScore = sum . map getMatrixScore

main :: IO ()
main = adventOfCode 13 day13parse (day13 getMatrixScoreA) (day13 getMatrixScoreB)
