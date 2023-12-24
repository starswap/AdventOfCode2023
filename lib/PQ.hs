module PQ where

import qualified Data.Map as M

newtype PQ v = PQ (M.Map Int [v]) deriving Show

insert :: Int -> v -> PQ v -> PQ v 
insert cost value (PQ m) = PQ (M.insertWith (++) cost [value] m)

deleteFindMin :: PQ v -> ((Int, v), PQ v)
deleteFindMin (PQ m) = case vs of 
    []   -> deleteFindMin (PQ (M.deleteMin m))
    x:xs -> ((k, x), PQ (M.insert k xs m))   
  where (k, vs) = M.findMin m 

singleton :: Int -> v -> PQ v
singleton k x = PQ (M.fromList [(k, [x])])
