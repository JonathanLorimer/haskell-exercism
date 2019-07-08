module ProteinTranslation(proteins) where

import Data.List.Split (chunksOf)
import Data.Maybe
import qualified Data.Map as M

proteins :: String -> Maybe [String]
proteins = convert . (chunksOf 3)

convert :: [String] -> Maybe [String]
convert = maybeList
        . fmap ((flip M.lookup) proteinMap)
        . takeWhile (\x -> (x /= "UAA") && (x /= "UAG") && (x /= "UGA"))

maybeList :: Eq a => [Maybe a] -> Maybe [a]
maybeList xs = if any (== Nothing) xs
               then Nothing
               else Just (catMaybes xs)

proteinMap :: M.Map String String
proteinMap = M.fromList [ ("AUG","Methionine")
                      , ("UUU", "Phenylalanine")
                      , ("UUC", "Phenylalanine")
                      , ("UUA", "Leucine")
                      , ("UUG", "Leucine")
                      , ("UCU", "Serine")
                      , ("UCC", "Serine")
                      , ("UCA", "Serine")
                      , ("UCG", "Serine")
                      , ("UAU", "Tyrosine")
                      , ("UAC", "Tyrosine")
                      , ("UGU", "Cysteine")
                      , ("UGC", "Cysteine")
                      , ("UGG", "Tryptophan") ]