module Matrix
    ( Matrix
    , cols
    , column
    , flatten
    , fromList
    , fromString
    , reshape
    , row
    , rows
    , shape
    , transpose
    ) where

import Control.Arrow ((&&&))
import Control.Monad
import Data.List (lines, words)
import Data.Maybe (maybe)
import Data.Vector (Vector)
import qualified Data.Vector as V

type Matrix a = Vector (Vector a)

safeHead :: Vector a -> Maybe a
safeHead v = if null v then Nothing else Just $ V.head v

cols :: Matrix a -> Int
cols = maybe 0 V.length . safeHead

column :: Int -> Matrix a -> Vector a
column x = fmap (V.! (x - 1))

flatten :: Matrix a -> Vector a
flatten = join

fromList :: [[a]] -> Matrix a
fromList = V.fromList . fmap V.fromList

fromString :: Read a => String -> Matrix a
fromString = fromList . fmap (fmap read . words) . lines

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (r,_) = fromList . go r . V.toList . flatten
  where
    go :: Int -> [a] -> [[a]]
    go _ [] = []
    go n xs = take n xs : go n (drop n xs)

row :: Int -> Matrix a -> Vector a
row n = (V.! (n - 1))

rows :: Matrix a -> Int
rows = V.length

shape :: Matrix a -> (Int, Int)
shape = rows &&& cols

transpose :: Matrix a -> Matrix a
transpose matrix = V.fromList . fmap (`column` matrix) $ [1 .. cols matrix]
