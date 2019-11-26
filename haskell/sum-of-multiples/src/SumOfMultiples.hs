module SumOfMultiples (sumOfMultiples) where

import Data.List
import Data.Foldable

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples fact lim = foldl' (\s -> maybe s (s +)) 0
                   $ fmap asum
                   $ do
                      a <- [0..(lim - 1)]
                      pure $ do
                        f <- multOf <$> fact
                        pure (f a)

multOf :: Integer -> Integer -> Maybe Integer
multOf f m
  | m == 0 || f == 0 = Nothing
  | otherwise        = if m `mod` f == 0 then Just m else Nothing

