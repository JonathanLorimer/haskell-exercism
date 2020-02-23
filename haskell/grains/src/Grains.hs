module Grains (square, total) where

square :: Int -> Maybe Integer
square n
  | n > 0 && n < 65 = Just $ 2^(n - 1)
  | otherwise = Nothing

total :: Integer
total = case traverse square [1..64] of
          Just x  -> sum x
          Nothing -> error "should never hit this case"
