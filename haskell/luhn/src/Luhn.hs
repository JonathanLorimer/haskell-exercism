module Luhn (isValid) where

import Data.Char

isValid :: String -> Bool
isValid n = all isDigit clean
          && length clean > 1
          && check clean
  where
    check = (== 0)
          . (`mod` 10)
          . sum
          . zipWith ($) (cycle [id, modProd 9 2])
          . map digitToInt
          . reverse
    clean = filter (not . isSpace) n

modProd :: Int -> Int -> Int -> Int
modProd d m v = if v == d then v else m * v `mod` d

-- $> isValid "055 444 285"
