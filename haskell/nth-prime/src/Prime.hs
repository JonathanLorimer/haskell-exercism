module Prime (nth) where

nth :: Int -> Maybe Integer
nth n
  | n <= 0    = Nothing
  | otherwise = Just $ sieve [2..] !! (n - 1)
      where sieve (p:xs) =
             p : sieve [x | x <- xs, x `mod` p /= 0]
