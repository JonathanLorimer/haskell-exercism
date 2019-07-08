module Triangle (rows) where

import Data.List (foldl')

rows :: Int -> [[Integer]]
rows r = take r (iterate go [1])
    where
        go row = foldl' f [] (0 : row)
        f []     element = [element]
        f (x:xs) element = element : (x + element) : xs
