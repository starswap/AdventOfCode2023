module Main where

-- Mine
import Common (adventOfCode, Input)
import Direction (Direction(L, R, U, D))
import Parsers (parseWellFormed, parsePositiveInteger)

-- Base
import Data.Char (digitToInt, ord, toUpper, isNumber)
import Data.List (sort, groupBy)
import Text.Gigaparsec (Parsec, (<|>))
import Text.Gigaparsec.Combinator (sepBy, exactly)
import Text.Gigaparsec.Char (endOfLine, char, space, hexDigit, string)

type Coords = (Int, Int)
data Instruction = Instruction Direction Int deriving Show
data Edge = Edge {row :: Int, minCol :: Int, maxCol :: Int} deriving (Show, Eq, Ord)

infty :: Int
infty = maxBound `div` 2

parseDirection :: Parsec Direction
parseDirection = L <$ char 'L' <|> R <$ char 'R' <|> U <$ char 'U' <|> D <$ char 'D'

fromHex :: String -> Int
fromHex = snd . foldr (\digit (pos, ans) -> (16 * pos, pos * hexDigitToInt digit + ans)) (1, 0)
  where hexDigitToInt :: Char -> Int
        hexDigitToInt x
          | isNumber x   = digitToInt x
          | otherwise    = 10 + ord (toUpper x) - ord 'A'

parseInstructionA :: Parsec Instruction
parseInstructionA = do
    dir <- parseDirection; space
    steps <- parsePositiveInteger; string " (#"
    _ <- exactly 6 hexDigit; char ')'
    return (Instruction dir steps)

parseInstructionB :: Parsec Instruction
parseInstructionB = do
    _ <- parseDirection; space
    _ <- parsePositiveInteger; string " (#"
    hexDistance <- exactly 5 hexDigit
    hexDirection <- hexDigit; char ')'
    return (Instruction (toEnum (digitToInt hexDirection)) (fromHex hexDistance))

day18parse :: Parsec Instruction -> Input -> [Instruction]
day18parse parseInstruction = parseWellFormed (sepBy parseInstruction endOfLine)

processInstruction :: Coords -> [Edge] -> Instruction -> (Coords, [Edge])
processInstruction (r, c) edges (Instruction L steps) = ((r, c - steps), Edge r (c - steps) c : edges)
processInstruction (r, c) edges (Instruction R steps) = ((r, c + steps), Edge r c (c + steps) : edges)
processInstruction (r, c) edges (Instruction U steps) = ((r - steps, c), edges)
processInstruction (r, c) edges (Instruction D steps) = ((r + steps, c), edges)

edges :: [Instruction] -> [Edge]
edges = snd . foldl (uncurry processInstruction) ((0, 0), [])

edgeLength :: Edge -> Int
edgeLength e = maxCol e - minCol e + 1

-- might be able to move the edgeLength inside to the base cases
updateActiveEdges :: [Edge] -> Edge -> ([Edge], Int)
updateActiveEdges [] newEdge = ([newEdge], 0)
updateActiveEdges (e:es) newEdge
  | maxCol newEdge + 1 == minCol e                       = (Edge (row e) (minCol newEdge) (maxCol e):es, 0)
  | minCol newEdge == maxCol e + 1                       = (Edge (row e) (minCol e) (maxCol newEdge):es, 0)
  | maxCol newEdge == minCol e                           = (fst (updateActiveEdges es (Edge (row e) (minCol newEdge) (maxCol e))), snd (updateActiveEdges es (Edge (row e) (minCol newEdge) (maxCol e))) -1)
  | minCol newEdge == maxCol e                           = (fst (updateActiveEdges es (Edge (row e) (minCol e) (maxCol newEdge))), snd (updateActiveEdges es (Edge (row e) (minCol e) (maxCol newEdge))) -1)
  | maxCol newEdge < minCol e                            = (newEdge:e:es, 0)
  | minCol e == minCol newEdge && maxCol e == maxCol newEdge = (es, (- edgeLength newEdge))
  | minCol e == minCol newEdge && maxCol e < maxCol newEdge  = let (f, s) = updateActiveEdges es (Edge (row e) (maxCol e + 1) (maxCol newEdge)) in (f, s - edgeLength e)
  | minCol e == minCol newEdge && maxCol e > maxCol newEdge  = (Edge (row e) (maxCol newEdge) (maxCol e):es, (- edgeLength newEdge))
  | maxCol e == maxCol newEdge && minCol e > minCol newEdge  = let (f, s) = updateActiveEdges es (Edge (row e) (minCol newEdge) (minCol e - 1)) in (f, s - edgeLength e)
  | maxCol e == maxCol newEdge && minCol e < minCol newEdge  = (Edge (row e) (minCol e) (minCol newEdge):es, (- edgeLength newEdge))
  | minCol e < minCol newEdge && maxCol e > maxCol newEdge   = (Edge (row e) (minCol e) (minCol newEdge):(Edge (row e) (maxCol newEdge) (maxCol e)):es, (- edgeLength newEdge))
  | otherwise                                        = let (es', d) = updateActiveEdges es newEdge in (e:es', d)

sweepLine :: [[Edge]] -> [Edge] -> Int -> Int
sweepLine [] [] _  = 0
sweepLine (ees@(e:_):fs) activeEdges r
  = theseRows + scd + sweepLine fs activeEdges' (row e + 1)
  where activeNodes  = sum . map edgeLength $ activeEdges
        (activeEdges', scd) = foldl (\(ae, sc) e -> let (ae', sc') = updateActiveEdges ae e in (ae', sc + sc' +  edgeLength e)) (activeEdges, 0) ees
        theseRows    = activeNodes * (row e - r + 1)
sweepLine _ _ _ = error "Error: The hole was not a single closed polygon"

day18 :: [Instruction] -> Int
day18 instrs = sweepLine (groupBy (\a b -> row a == row b) . sort . edges $ instrs) [] (-infty)

main :: IO ()
main = adventOfCode 18 id (day18 . day18parse parseInstructionA) (day18 . day18parse parseInstructionB)
