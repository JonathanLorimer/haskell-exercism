module SumOfMultiples (sumOfMultiples) where

import Data.Set as S
import Data.Foldable

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples fact lim = S.foldl' (+) 0 . S.filter (< lim) $ mults
  where mults = fromList $ (*) <$> [1..lim] <*> fact


