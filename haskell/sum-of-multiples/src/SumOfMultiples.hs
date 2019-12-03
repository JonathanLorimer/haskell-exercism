module SumOfMultiples (sumOfMultiples) where

import Data.List

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples fact lim = sum . nub . takeWhile (< lim) . sort $ mults
  where mults = (*) <$> [1..lim] <*> fact


