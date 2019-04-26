module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance xs ys = if length xs == length ys
                 then  Just . sum $ zipWith geneCheck xs ys
                 else Nothing

geneCheck :: Char -> Char -> Int
geneCheck c1 c2
    | c1 == c2 = 0
    | otherwise = 1