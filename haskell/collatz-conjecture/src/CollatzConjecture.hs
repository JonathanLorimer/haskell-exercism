module CollatzConjecture
  ( collatz
  )
where

collatz :: Integer -> Maybe Integer
collatz n
  | n <= 0    = Nothing
  | otherwise = Just 
              . fromIntegral 
              . length 
              . (takeWhile notOne) 
              . (iterate iterator) 
              . fromInteger 
              $ n
      where
        iterator :: Double -> Double
        iterator num = if (round num :: Integer) `rem` 2 == 0
                then num / 2
                else num * 3 + 1
        notOne :: Double -> Bool
        notOne = ( /= 1)
