module String where

import Data.List (tails)

findSubstring :: String -> String -> [Int]
findSubstring sub super = [idx | (idx, internal) <- zip [0..] (tails super), take l internal == sub]  
  where l = length sub