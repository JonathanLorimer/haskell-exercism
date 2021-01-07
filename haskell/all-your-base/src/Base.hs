{-# LANGUAGE MultiWayIf #-}
module Base (Error(..), rebase) where

import Data.List (find)

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
    deriving (Show, Eq)

basify :: Integral a => a -> a -> [a]
basify b v = go b v []
  where
    go :: Integral a => a -> a -> [a] -> [a]
    go base value acc
      | value < base = value : acc
      | otherwise = let (value', diff) = quotRem value base in go base value' (diff : acc)

isOutsideRange :: Integral a => a -> a -> Bool
isOutsideRange base val = val < 0 || base <= val

rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase inputDigits =
  let withReverseIndices = zip inputDigits [length inputDigits - 1, length inputDigits - 2 .. 0]
      absoluteValue = foldr (\(val, pow) acc -> val * (inputBase ^ pow) + acc) 0 withReverseIndices
      outOfBoundsDigit = find (isOutsideRange inputBase) inputDigits
  in if | inputBase <= 1 -> Left InvalidInputBase
        | outputBase <= 1 -> Left InvalidOutputBase
        | otherwise ->
          case outOfBoundsDigit of
             Just x -> Left $ InvalidDigit x
             Nothing -> Right $ if absoluteValue == 0
                                  then []
                                  else basify outputBase absoluteValue
