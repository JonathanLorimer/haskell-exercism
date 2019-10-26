module BinarySearch (find) where

import Data.Array
import Data.List hiding (find)

find :: Ord a => Array Int a -> a -> Maybe Int
find arr x = search x b . listArray b . sort . elems $ arr
  where
    b = bounds arr

search :: (Ord a) => a -> (Int, Int) -> Array Int a -> Maybe Int
search v bs@(l, u) a = if l > u then Nothing
     else case getMid bs of
            Left (ix1, ix2)
                | (a ! ix1) == v -> Just ix1
                | (a ! ix2) == v -> Just ix2
                | otherwise      -> Nothing
            Right mid ->
              case compare v (a ! mid) of
                  EQ -> Just mid
                  LT -> search v (l, mid) a
                  GT -> search v (mid, u) a

getMid :: (Int, Int) -> Either (Int, Int) Int
getMid (a, b) =
   if abs (b - a) <= 1
        then Left (a, b)
        else Right $ a + ((b - a) `div` 2)
