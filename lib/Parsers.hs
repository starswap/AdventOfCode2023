module Parsers where

import Text.Gigaparsec (some, Parsec)
import Text.Gigaparsec.Char (digit)

parsePositiveInteger :: Parsec Int
parsePositiveInteger = fmap read (some digit)
