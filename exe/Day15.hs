module Main where

-- Base
import Control.Monad (foldM)
import Control.Monad.ST (runST, ST)

import Data.Array.ST (STArray, newArray, readArray, writeArray, getElems)
import Data.Char (ord, isAlpha)
import qualified Data.Text as T

-- Mine
import Common (adventOfCode)

type Instruction = String
data Lens = Lens {label :: String, fLength :: Int}

hash :: Instruction -> Int
hash = foldl step 0
  where step :: Int -> Char -> Int
        step h char = ((h + ord char) * 17) `mod` 256

empty :: ST s (STArray s Int [Lens])
empty = newArray (0, 255) []

processMinus :: STArray s Int [Lens] -> Int -> [Lens] -> [Lens] -> ST s ()
processMinus hm idx before []     = return ()
processMinus hm idx before (_:xs) = writeArray hm idx (before ++ xs)

processEquals :: STArray s Int [Lens] -> Int -> Int -> String -> [Lens] -> [Lens] -> ST s ()
processEquals hm idx focalLength lbl before []     = writeArray hm idx (before ++ [Lens lbl focalLength])
processEquals hm idx focalLength lbl before (_:xs) = writeArray hm idx (before ++ [Lens lbl focalLength] ++ xs)

processInstruction ::  STArray s Int [Lens] -> Instruction -> ST s (STArray s Int [Lens])
processInstruction hm instr = do
  box <- readArray hm idx
  let (before, after) = break (\x -> label x == lbl) box
  case op of
    '-' -> processMinus hm idx before after
    '=' -> processEquals hm idx focalLength lbl before after
  return hm
  where
    (lbl, op:num) = span isAlpha instr
    focalLength = read num :: Int
    idx = hash lbl

processAllInstructions :: [Instruction] -> [[Lens]]
processAllInstructions instrs = runST $ do
  initArray <- empty
  finalArray <- foldM processInstruction initArray instrs
  getElems finalArray

score :: [[Lens]] -> Int
score boxes = sum (zipWith scoreOneBox [1..] boxes)

scoreOneBox :: Int -> [Lens] -> Int
scoreOneBox boxNo contents = boxNo * sum (map (\(i, Lens _ len) -> i * len) indexed)
  where
    indexed = zip [1..] contents

day15a :: [Instruction] -> Int
day15a = sum . map hash

day15b :: [Instruction] -> Int
day15b = score . processAllInstructions

main :: IO ()
main = adventOfCode 15 (map T.unpack . T.split (== ',') . T.pack) day15a day15b
