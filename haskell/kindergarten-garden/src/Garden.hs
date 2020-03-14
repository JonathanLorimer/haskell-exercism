module Garden
    ( Plant (..)
    , garden
    , lookupPlants
    ) where

import qualified Data.Map   as M
import           Data.Maybe

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

type Garden = M.Map String [Plant]

plantify :: Char -> Plant
plantify 'C' = Clover
plantify 'G' = Grass
plantify 'R' = Radishes
plantify 'V' = Violets
plantify _   = error "no such plant"

garden :: [String] -> String -> Garden
garden students plants =
  let [f, s] = lines plants
      first = doubles f
      second = doubles s
   in M.fromList
   $ fmap (fmap plantify) <$>
   zip students (zipWith (++) first second)

doubles :: [a] -> [[a]]
doubles []       = []
doubles [_]      = []
doubles (x:y:xs) = [x, y] : doubles xs

lookupPlants :: String -> Garden -> [Plant]
lookupPlants s g = fromJust $ M.lookup s g

