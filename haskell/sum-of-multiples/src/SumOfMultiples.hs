module SumOfMultiples (sumOfMultiples) where

import Data.Set as S

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples fact lim = S.foldl' (+) 0 . S.filter (< lim) $ mults
  where mults = fromList $ (*) <$> [1..lim] <*> fact


