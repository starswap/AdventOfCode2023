module Common where
import System.Environment (getArgs)
import System.IO
import Data.Array (Array, listArray)

type Line = String
type Input = String

adventOfCode :: (Show a, Show b) => Int -> (Input -> c) -> (c -> a) -> (c -> b) -> IO ()
adventOfCode dayNo parse partA partB = do
    args <- getArgs
    handle <- openFile (head args) ReadMode
    contents <- hGetContents handle
    putStrLn ("--- Day " <> show dayNo <> " Results ---")
    putStr "Part A Answer: " 
    print (partA . parse $ contents)
    putStr "Part B Answer: " 
    print (partB . parse $ contents)

arrayParse :: Input -> Array (Int, Int) Char
arrayParse input = listArray ((1, 1), (h, w)) (concat ls)
  where 
    ls@(l:_) = lines input
    w = length l
    h = length ls