module Common where
import System.Environment (getArgs)
import System.IO

type Line = String
type Input = String

adventOfCode :: Int -> (String -> String) -> (String -> String) -> IO ()
adventOfCode dayNo partA partB = do
    args <- getArgs
    handle <- openFile (head args) ReadMode
    contents <- hGetContents handle
    putStrLn ("--- Day " <> show dayNo <> " Results ---")
    putStr "Part A Answer: " 
    putStrLn $ partA contents
    putStr "Part B Answer: " 
    putStrLn $ partB contents
    