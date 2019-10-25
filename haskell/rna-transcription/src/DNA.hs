module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA = traverse parse

parse :: Char -> Either Char Char
parse 'G' = Right 'C'
parse 'C' = Right 'G'
parse 'T' = Right 'A'
parse 'A' = Right 'U'
parse  c  = Left c
