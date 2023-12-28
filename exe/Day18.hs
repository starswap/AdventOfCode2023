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

type Correction = Int
type Coords = (Int, Int)
data Instruction = Instruction Direction Int deriving Show
data Edge = Edge {row :: Int, minCol :: Int, maxCol :: Int} deriving (Show, Eq, Ord)

infty :: Int
infty = maxBound `div` 2

fromHex :: String -> Int
fromHex = snd . foldr (\digit (pos, ans) -> (16 * pos, pos * hexDigitToInt digit + ans)) (1, 0)
  where hexDigitToInt :: Char -> Int
        hexDigitToInt x
          | isNumber x   = digitToInt x
          | otherwise    = 10 + ord (toUpper x) - ord 'A'

parseDirection :: Parsec Direction
parseDirection = L <$ char 'L' <|> R <$ char 'R' <|> U <$ char 'U' <|> D <$ char 'D'

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

-- Apply an edge to the active edge set, turning on or off pixels
-- as necessary. Returns the new active edge set and a "correction factor"
-- The correction factor deals with the issue that if an edge enlarges the
-- active pixel set we need to include that in the current row, whereas
-- if the edge reduces the active pixel set, we want to count the current
-- row as if that hasn't happened yet. We do this by assuming we can
-- count an edge with the rows that come before it. This means that any
-- removals by this row are ignored for the purpose of calculating its
-- score which is what we want, but any additions are not. We add those
-- in separately via the correction factor. 
-- Note the definition of <$> for a tuple maps the function over the snd.
updateActiveEdges :: [Edge] -> Edge -> ([Edge], Correction)
updateActiveEdges [] newEdge                                 = ([newEdge], edgeLength newEdge)                                                  -- Completely new edge; correction factor = the amount we added
updateActiveEdges (e:es) newEdge
  | maxCol newEdge < minCol e                                = (newEdge:e:es, edgeLength newEdge)                                               -- Completely new edge; correction factor = the amount we added
  | minCol e == minCol newEdge && maxCol e == maxCol newEdge = (es, 0)                                                                          -- Completely deleting an existing edge
  | maxCol newEdge == minCol e                               = (+ (edgeLength newEdge - edgeLength newOld - 1)) <$> updateActiveEdges es newOld -- Overlaps an existing edge, try to insert the new combined edge. Reduce correction factor to just the edge we started inserting, minus 1 to account for the overlap
  | minCol newEdge == maxCol e                               = (+ (edgeLength newEdge - edgeLength oldNew - 1)) <$> updateActiveEdges es oldNew -- Overlaps an existing edge, try to insert the new combined edge. Reduce correction factor to just the edge we started inserting, minus 1 to account for the overlap
  | minCol e == minCol newEdge && maxCol e > maxCol newEdge  = (oldAfter:es, 0)                                                                 -- Fully enclosed by an existing edge; turn that part of it off
  | maxCol e == maxCol newEdge && minCol e < minCol newEdge  = (oldBefore:es, 0)                                                                -- Fully enclosed by an existing edge; turn that part of it off
  | minCol e < minCol newEdge  && maxCol e > maxCol newEdge  = (oldBefore:oldAfter:es, 0)                                                       -- Fully enclosed by an existing edge; turn that part of it off
  | otherwise                                                = let (es', d) = updateActiveEdges es newEdge in (e:es', d)                        -- Haven't yet found the right place where this edge should go in the sorted activeEdges
  where newOld    = Edge (row e) (minCol newEdge) (maxCol e)
        oldNew    = Edge (row e) (minCol e) (maxCol newEdge)
        oldBefore = Edge (row e) (minCol e) (minCol newEdge)
        oldAfter  = Edge (row e) (maxCol newEdge) (maxCol e)

-- Sweep from the top of the picture (-ve infinity)
-- to the bottom, keeping track of which columns are currently 'on' in terms
-- of the 'active' horizontal "edges" that caused them to be on.
-- (process all edges at this row in one go, accumulating correction factors)
-- ####### <- previousRows = 0 * infty  + CF of 7
-- #.....#
-- ###...# <- previousRows = 7 * 2 (14) + CF of 0
-- ..#...#
-- ..#...#
-- ###.### <- previousRows = 5 * 3 (15) + CF of 2
-- #...#..
-- ##..### <- previousRows = 5 * 2 (10) + CF of 2
-- .#....#
-- .###### <- previousRows = 6 * 2 (12) + CF of 0
sweepLine :: [[Edge]] -> [Edge] -> Int -> Int
sweepLine [] [] _              = 0
sweepLine (ees@(e:_):fs) aes r = previousRows + sweepLine fs aes' (row e)
  where previousRows             = (sum . map edgeLength $ aes) * (row e - r) + correctionFactor
        (aes', correctionFactor) = foldl (\(ae, corr) edge -> (+ corr) <$> updateActiveEdges ae edge) (aes, 0) ees 
sweepLine _ _ _ = error "Error: The hole was not a single closed polygon"

day18 :: [Instruction] -> Int
day18 instrs = sweepLine (groupBy (\a b -> row a == row b) . sort . edges $ instrs) [] (-infty)

main :: IO ()
main = adventOfCode 18 id (day18 . day18parse parseInstructionA) (day18 . day18parse parseInstructionB)
