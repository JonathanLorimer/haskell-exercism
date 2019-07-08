module Triplet (tripletsWithSum) where

tripletsWithSum :: Int -> [(Int, Int, Int)]
tripletsWithSum x = [(a,b,c) | b <- [2..x], a <- [2..b], let c = x - (b + a) , a^2 + b^2 == c^2]
