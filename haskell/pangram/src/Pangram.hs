module Pangram (isPangram) where
import Data.Char (toLower)

isPangram :: String -> Bool
isPangram text = all (`elem` (toLower <$> text)) ['a'..'z']