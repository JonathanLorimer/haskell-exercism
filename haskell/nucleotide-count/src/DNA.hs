module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map, fromList, adjust)
import Control.Monad (foldM)

data Nucleotide = A | C | G | T
  deriving (Eq, Ord, Show)

nucleotideCounts :: String
                 -> Either String (Map Nucleotide Int)
nucleotideCounts =
  foldM parse (fromList [(A, 0), (C, 0), (G, 0), (T,0)])

parse :: Map Nucleotide Int
      -> Char
      -> Either String (Map Nucleotide Int)
parse m 'A' = Right $ adjust (+ 1) A m
parse m 'C' = Right $ adjust (+ 1) C m
parse m 'G' = Right $ adjust (+ 1) G m
parse m 'T' = Right $ adjust (+ 1) T m
parse _  _  = Left "invalid char"
