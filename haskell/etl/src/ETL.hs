module ETL (transform) where

import qualified Data.Map as M
import Data.Map (Map)
import Data.Char

transform :: Map a String -> Map Char a
transform = swapKV . M.map (fmap toLower)

swapKV :: (Ord v, Foldable f) => Map k (f v) -> Map v k
swapKV = M.foldrWithKey swap M.empty
  where
    swap k v m = foldr (\x m' -> M.insert x k m') m v
