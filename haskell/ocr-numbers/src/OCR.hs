module OCR (convert) where

import Data.Maybe
import Data.List

convert :: String -> String
convert = concat . (intersperse ",") . (map ocrLine) . (groupLines 4) . lines

ocrLine :: LineGrid -> String
ocrLine (LineGrid xss) = parseLine xss >>= ocrDigit
  
newtype LineGrid = LineGrid [String]

-- | Form groups of n lines:
groupLines :: Int -> [String] -> [LineGrid]
groupLines _ [] = []
groupLines n xs = LineGrid (take n xs) : groupLines n (drop n xs)

newtype DigitGrid = DigitGrid [String]

-- | Try to divide the input into a sequence of 4x3 (rows x cols) matrices.
parseLine :: [String] -> [DigitGrid]
parseLine [w0:w1:w2:ws, x0:x1:x2:xs, y0:y1:y2:ys, z0:z1:z2:zs] =
  DigitGrid [ [w0, w1, w2]
            , [x0, x1, x2]
            , [y0, y1, y2]
            , [z0, z1, z2] ] : parseLine [ws, xs, ys, zs]
parseLine _ = []

-- | Try to recognize a single digit.
ocrDigit :: DigitGrid -> String
ocrDigit (DigitGrid xs)
  | xs == repZero  = "0"
  | xs == repOne   = "1"
  | xs == repTwo   = "2"
  | xs == repThree = "3"
  | xs == repFour  = "4"
  | xs == repFive  = "5"
  | xs == repSix   = "6"
  | xs == repSeven = "7"
  | xs == repEight = "8"
  | xs == repNine  = "9"    
  | otherwise      = "?"

-- * presentation of the numbers:
repZero = [ " _ "
          , "| |"
          , "|_|"
          , "   "
          ]

repOne  = [ "   "
          , "  |"
          , "  |"
          , "   "
          ]          

repTwo  = [ " _ "
          , " _|"
          , "|_ "
          , "   "
          ]

repThree =[ " _ "
          , " _|"
          , " _|"
          , "   "
          ]

repFour  =[ "   "
          , "|_|"
          , "  |"
          , "   "
          ]

repFive  =[ " _ "
          , "|_ "
          , " _|"
          , "   "
          ]

repSix   =[ " _ "
          , "|_ "
          , "|_|"
          , "   "
          ]
          
repSeven =[ " _ "
          , "  |"
          , "  |"
          , "   "
          ]
          
repEight =[ " _ "
          , "|_|"
          , "|_|"
          , "   "
          ]

repNine  =[ " _ "
          , "|_|"
          , " _|"
          , "   "
          ]
