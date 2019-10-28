module Sieve (primesUpTo) where

-- You should not use any of the division operations when implementing
-- the sieve of Eratosthenes.
import Prelude hiding (div, mod, divMod, rem, quotRem, quot, (/))
import qualified Data.Map as Map

primesUpTo :: Integer -> [Integer]
primesUpTo n = go [2..n] Map.empty
  where
    go [] _ = []
    go (x:xs) table =
      case Map.lookup x table of
        Nothing -> x : go xs (Map.insert (x*x) [x] table)
        Just facts -> go xs (foldl reinsert (Map.delete x table) facts)
          where
            reinsert table' prime = Map.insertWith (++) (x+prime) [prime] table'

