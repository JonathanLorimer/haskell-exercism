{-# LANGUAGE TupleSections #-}

module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map, adjust, fromList)

data Nucleotide = A | C | G | T
  deriving (Eq, Ord, Enum, Bounded, Show)

nucleotideCounts :: String
                 -> Either String (Map Nucleotide Int)
nucleotideCounts s = foldr (adjust (+1)) zero <$> traverse parse s
  where
    zero = fromList $ (,0) <$> [minBound ..]

parse :: Char
      -> Either String Nucleotide
parse 'A' = Right A
parse 'C' = Right C
parse 'G' = Right G
parse 'T' = Right T
parse  _  = Left "invalid char"
