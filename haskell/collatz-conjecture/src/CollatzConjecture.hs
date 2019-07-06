module CollatzConjecture
    ( collatz
    )
where

import           Data.List                      ( unfoldr )

collatz :: Integer -> Maybe Integer
collatz n
    | n <= 0    = Nothing
    | otherwise = Just . fromIntegral . length . unfoldr go . fromInteger $ n
  where
    go :: Double -> Maybe (Double, Double)
    go num | num == 1               = Nothing
           | round num `rem` 2 == 0 = Just (num, num / 2)
           | otherwise              = Just (num, num * 3 + 1)


-- Take any positive integer n. If n is even, divide n by 2 to get n / 2. If n is odd, multiply n by 3 and add 1 to get 3n + 1. Repeat the process indefinitely. The conjecture states that no matter which number you start with, you will always reach 1 eventually.

-- Given a number n, return the number of steps required to reach 1.
