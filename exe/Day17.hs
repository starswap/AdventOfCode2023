module Main where

-- Mine
import Common (adventOfCode, Input)
import Array (inBounds)
import Tuple ((+++))
import PQ (PQ, deleteFindMin, insert, singleton)

-- Base
import Data.Array (Array, (!), bounds, listArray)
import Data.Char (digitToInt)
import qualified Data.Map as M

type Coords = (Int, Int)
type Grid = Array (Int, Int) Int
type StateSpaceV = (Coords, Int, Direction)
type CostMap = M.Map StateSpaceV Int
type DijkstraState = (PQ StateSpaceV, CostMap)
type CrucibleRules = Int -> DijkstraState -> DijkstraState -> DijkstraState -> Coords -> (Int, Int) -> Maybe DijkstraState

data Direction = R | D | L | U deriving (Eq, Ord, Enum, Show)

delta :: Direction -> (Int, Int)
delta L = (0, -1)
delta R = (0, 1)
delta U = (-1, 0)
delta D = (1, 0)

leftTurn :: Direction -> Direction
leftTurn x = toEnum $ (fromEnum x - 1) `mod` 4

rightTurn :: Direction -> Direction
rightTurn x = toEnum $ (fromEnum x + 1) `mod` 4

basicCrucibles :: CrucibleRules
basicCrucibles x noStraight _ allOptions (r, c) (h, w) 
  | (r, c) == (h, w)            = Nothing              -- done
  | x == 3                      = Just noStraight      -- have to turn
  | otherwise                   = Just allOptions      -- free choice

ultraCrucibles :: CrucibleRules
ultraCrucibles x noStraight onlyStraight allOptions (r, c) (h, w)
  | (r, c) == (h, w) && x >= 4  = Nothing              -- done
  | x <= 3                      = Just onlyStraight    -- can't turn yet
  | x == 10                     = Just noStraight      -- have to turn
  | otherwise                   = Just allOptions      -- free choice

updateCost :: Grid -> DijkstraState -> StateSpaceV -> Int -> DijkstraState
updateCost grid old@(pq, costMap) vertex@(coords, _, _) newCost
  | not . inBounds grid $ coords                        = old -- out of bounds so no updates  
  | newCost < M.findWithDefault maxBound vertex costMap = new -- better so update
  | otherwise                                           = old -- worse so don't update
  where new = (PQ.insert newCost vertex pq, M.insert vertex newCost costMap)

dijkstra :: CrucibleRules -> Grid ->  DijkstraState -> Int
dijkstra crucibleRules grid (pq, costMap)
    | cost /= costMap M.! v = dijkstra crucibleRules grid (pq', costMap) -- stale; skip
    | otherwise 
      = case newState of
          Nothing           -> cost                                      -- done
          Just state'       -> dijkstra crucibleRules grid state'        -- not done
    where
      (_, (h, w)) = bounds grid
      ((cost, v@((r, c), x, dir)), pq') = PQ.deleteFindMin pq

      straightVert = (r, c) +++ delta dir
      leftVert     = (r, c) +++ delta (leftTurn dir)
      rightVert    = (r, c) +++ delta (rightTurn dir)

      afterLeft    = updateCost grid (pq', costMap) (leftVert, 1, leftTurn dir) (cost + grid ! leftVert)  
      noStraight   = updateCost grid afterLeft (rightVert, 1, rightTurn dir) (cost + grid ! rightVert)  
      allOptions   = updateCost grid noStraight (straightVert, x + 1, dir) (cost + grid ! straightVert)  
      onlyStraight = updateCost grid (pq', costMap) (straightVert, x + 1, dir) (cost + grid ! straightVert)
      newState     = crucibleRules x noStraight onlyStraight allOptions (r, c) (h, w)

day17parse :: Input -> Grid
day17parse input = listArray ((1, 1), (h, w)) (map digitToInt (concat ls))
  where
    ls@(l:_) = lines input
    w = length l
    h = length ls

day17 :: CrucibleRules -> Grid -> Int
day17 crucibleRules grid = dijkstra crucibleRules grid (initPq, initCostMap)
  where
    startR      = ((1, 1), 0, R)
    startD      = ((1, 1), 0, D)
    initPq      = PQ.insert 0 startD (PQ.singleton 0 startR)
    initCostMap = M.fromList [(startR, 0), (startD, 0)]

main :: IO ()
main = adventOfCode 17 day17parse (day17 basicCrucibles) (day17 ultraCrucibles)
