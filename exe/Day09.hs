module Main where

-- Mine
import Common (adventOfCode, Input)

day09 :: ([Int] -> Int) -> Input -> Int
day09 extrapolate = sum . map (extrapolate . map read . words) . lines

differences :: [Int] -> [Int]
differences []       = error "Differences of Empty List"
differences l@(_:xs) = zipWith (-) xs l 

extrapolateForwards :: [Int] -> Int
extrapolateForwards l
  | all (== 0) l = 0
  | otherwise    = last l + extrapolateForwards (differences l)

extrapolateBackwards :: [Int] -> Int
extrapolateBackwards l
  | all (== 0) l = 0
  | otherwise    = head l - extrapolateBackwards (differences l)

main :: IO ()
main = adventOfCode 9 id (day09 extrapolateForwards) (day09 extrapolateBackwards)
