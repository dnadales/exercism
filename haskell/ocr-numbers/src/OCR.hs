module OCR (convert) where

import Data.Maybe

convert :: String -> String
convert = fromMaybe "?" . (ocrDigit . lines)
--  = error "You need to implement this function."

-- | Try to recognize a single digit.
ocrDigit :: [String] -> Maybe String
ocrDigit xs 
  | xs == repZero = Just "0"
  | xs == repOne  = Just "1"
  | otherwise     = Nothing

-- Representation of the numbers:
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
