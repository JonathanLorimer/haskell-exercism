-- module Triangle (rows) where

import Data.List (unfoldr)

rows :: Int -> [[Integer]]
rows 0 = 0
rows n = 

getRow :: Int -> [Int]
getRow n = 

getPascals :: Int -> Int -> [Int]
getPascals y x = 

pascal x y
    | x == 0 = 1
    | x == y = 1
    | otherwise = (pascal x (y - 1)) + pascal (x - 1) (y - 1)


--   | 0   1   2   3   4   5   6
-- --+--------------------------
-- 0 | 1
-- 1 | 1   1
-- 2 | 1   2   1
-- 3 | 1   3   3   1
-- 4 | 1   4   6   4   1
-- 5 | 1   5  10  10   5   1
-- 6 | 1   6  15  20  15   6   1