module CollatzConjecture
  ( collatz
  )
where

import Data.List (genericLength)

collatz :: Integer -> Maybe Integer
collatz n
  | n <= 0    = Nothing
  | otherwise = Just
              . genericLength
              . takeWhile (/= 1)
              . iterate iterator
              $ n
      where
        iterator :: Integer -> Integer
        iterator num = if even num
                then round $ fromInteger num / 2
                else num * 3 + 1
