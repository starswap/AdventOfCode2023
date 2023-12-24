module Tuple where

(+++) :: (Int, Int) -> (Int, Int) -> (Int, Int)
(x, y) +++ (x', y') = (x + x', y + y')
