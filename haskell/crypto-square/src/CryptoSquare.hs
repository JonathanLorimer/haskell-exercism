module CryptoSquare (encode) where
import Data.Char (toLower)
import Data.List (transpose, intercalate)


encode :: String -> String
encode xs = intercalate "\n" . transpose . (columnify rowSize') $ normalizedString
    where 
        normalizedString = normalize xs
        rowSize' = rowSize normalizedString

normalize :: String -> String
normalize = map toLower . filter noPunctuation
    where
        noPunctuation x = not (x `elem` "@%,.?!-:;\"\' ")

rowSize :: String -> Int
rowSize str = ceiling l
    where 
        l :: Double
        l = (sqrt $ fromIntegral $ length str)

columnify :: Int -> String -> [String]
columnify n s = go [] n s
        where
            go acc _ [] = reverse acc
            go acc num string = go ((take num string):acc) num (drop num string)

