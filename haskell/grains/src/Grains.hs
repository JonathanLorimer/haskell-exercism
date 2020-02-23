module Grains (square, total) where

powerTwo :: Int -> Integer
powerTwo n = 2 ^ (n - 1)

square :: Int -> Maybe Integer
square n
  | n > 0 && n < 65 = Just $ powerTwo n
  | otherwise = Nothing

total :: Integer
total = sum $ powerTwo <$> [1..64]
