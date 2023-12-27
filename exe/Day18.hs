module Main where

-- Mine
import Common (adventOfCode)
import Parsers (parseWellFormed, parsePositiveInteger)
import Tuple ((+++))
import Direction (Direction(L, R, U, D), delta, deltas, opposite)

-- Base
import qualified Data.Set as S
import Text.Gigaparsec (Parsec, (<|>), many)
import Text.Gigaparsec.Combinator (sepBy)
import Text.Gigaparsec.Char (endOfLine, char, space, satisfy)

type Coords = (Int, Int)
type Colour = String
data Instruction = Instruction {direction :: Direction, steps :: Int, colour :: Colour} deriving Show

parseDirection :: Parsec Direction
parseDirection = (L <$ char 'L') <|> (R <$ char 'R') <|> (U <$ char 'U') <|> (D <$ char 'D')

parseInstruction :: Parsec Instruction
parseInstruction = do
    dir <- parseDirection
    space
    steps <- parsePositiveInteger
    space
    char '('
    colour <- many (satisfy (/= ')'))
    char ')'
    return (Instruction dir steps colour)

day18parse :: Parsec [Instruction]
day18parse = sepBy parseInstruction endOfLine

initCoord :: Coords
initCoord = (0, 0)

processInstruction :: Coords -> S.Set Coords -> Instruction -> (Coords, S.Set Coords)
processInstruction pos set (Instruction _ 0 _)
  = (pos, S.insert pos set)
processInstruction pos set (Instruction dir steps colour) 
  = processInstruction (pos +++ delta dir) (S.insert pos set) (Instruction dir (steps - 1) colour)

buildEdging :: [Instruction] -> S.Set Coords
buildEdging = snd . foldl (uncurry processInstruction) (initCoord, S.empty) 

floodFill :: S.Set Coords -> Coords -> S.Set Coords
floodFill vis curr
  | S.member curr vis = vis
  | otherwise         = foldl floodFill (S.insert curr vis) (map (curr +++) deltas)

isInside :: S.Set Coords -> Coords -> Bool
isInside poly (r, c) = odd . length . filter (\(r', c') -> r == r' && c' < c) . S.toList $ poly

fillInside :: [Instruction] -> S.Set Coords -> S.Set Coords 
fillInside (i:j:_) edging = floodFill edging startLocation
  where
    afterOne = fst (processInstruction initCoord S.empty i)
    -- exactly one of these is inside the lagoon
    startChoiceA = afterOne +++ (delta . opposite . direction $ i) +++ (delta . direction $ j)
    startChoiceB = afterOne +++ (delta . direction $ i) +++ (delta . opposite . direction $ j)
    startLocation = if isInside edging startChoiceA then startChoiceA else startChoiceB
fillInside _ _ = error "Polygon doesn't have at least 2 edges."

day18a :: [Instruction] -> Int
day18a instrs = length wholeHole
  where edging    = buildEdging instrs
        wholeHole = fillInside instrs edging

day18b :: [Instruction] -> Int
day18b = undefined

main :: IO ()
main = adventOfCode 18 (parseWellFormed day18parse) day18a day18b
