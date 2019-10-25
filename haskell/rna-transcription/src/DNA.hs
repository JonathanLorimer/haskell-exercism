module DNA (toRNA) where

import Data.Foldable (foldrM)

toRNA :: String -> Either Char String
toRNA = foldrM parse mempty

parse :: Char -> String -> Either Char String
parse 'G' s = Right $ 'C' : s
parse 'C' s = Right $ 'G' : s
parse 'T' s = Right $ 'A' : s
parse 'A' s = Right $ 'U' : s
parse  c  _ = Left c
