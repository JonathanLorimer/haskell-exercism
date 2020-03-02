module ArmstrongNumbers (armstrong) where

import Data.List (unfoldr)

getDigits :: (Integral a) => a -> [a]
getDigits = reverse . unfoldr mod10
  where
    mod10 n = if n > 0
                 then Just (n `rem` 10, n `div` 10)
                 else Nothing

armstrong :: Integral a => a -> Bool
armstrong n = (\s -> n == s)
            . sum
            . (\(l, s) -> fmap (^l) s)
            . (\s -> (length s, s))
            $ getDigits n
