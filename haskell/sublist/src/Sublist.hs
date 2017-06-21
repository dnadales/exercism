module Sublist (Sublist(..), sublist) where

data Sublist = Equal | Sublist | Superlist | Unequal deriving (Eq, Show)

sublist xs ys
  | xs == ys = Equal
  | xs `gSublist` ys = Sublist
  | ys `gSublist` xs = Superlist
  | otherwise = Unequal

-- | Determines if the first list is a sublist of the second.
gSublist [] ys = True
gSublist (x:xs) [] = False
gSublist (x:xs) (y:ys) = (y:ys) `beginsWith` (x:xs) || (x:xs) `gSublist` ys

-- | Determines if the first list begins with the second.
beginsWith ys [] = True
beginsWith [] xs = False
beginsWith (y:ys) (x:xs) = x == y && ys `beginsWith` xs

