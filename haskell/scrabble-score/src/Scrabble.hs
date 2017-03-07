module Scrabble (scoreLetter, scoreWord) where

import qualified Data.Map.Strict as Map
import Data.Char
import Data.Maybe
import Data.List

scoreLetter :: Char -> Int
scoreLetter c = fromMaybe 0 $ Map.lookup (toUpper c) scores
  -- There is a lot of repetition in the code below:
  --
  -- where scores = Map.fromList (
  --         zip ['A', 'E', 'I', 'O', 'U', 'L', 'N', 'R', 'S', 'T'] (repeat 1)
  --         ++ zip ['D', 'G'] (repeat 2)
  --         ++ zip ['B', 'C', 'M', 'P'] (repeat 3)
  --         ++ zip ['F', 'H', 'V', 'W', 'Y'] (repeat 4)
  --         ++ zip ['K'] (repeat 5)
  --         ++ zip ['J', 'X'] (repeat 8)
  --         ++ zip ['Q', 'Z'] (repeat 10)
  --         )
  -- We could do this:
  -- where scores = Map.fromList $ concat $
  --         map (\(xs, s) -> zip xs (repeat s)) [ ("AEIOULNRST", 1)
  --                                             , ("DG", 2)
  --                                             , ("BCMP", 3)
  --                                             , ("FHVWY", 4)
  --                                             , ("K", 5)
  --                                             , ("JX", 8)
  --                                             , ("QZ", 10)
  --                                             ]
  -- But it is better to use the monadic `bind`!
    where scores = Map.fromList $ 
                   [ ("AEIOULNRST", 1)
                   , ("DG", 2)
                   , ("BCMP", 3)
                   , ("FHVWY", 4)
                   , ("K", 5)
                   , ("JX", 8)
                   , ("QZ", 10)
                   ] >>= \(xs, s) -> zip xs (repeat s)
scoreWord :: String -> Int
scoreWord = sum . map scoreLetter

-- | For better solutions see:
-- http://exercism.io/submissions/2df2f9affdf14a9eba8dc988b8714eff
