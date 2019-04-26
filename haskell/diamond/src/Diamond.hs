module Diamond (diamond) where
import Data.Char (toUpper)

diamond :: Char -> Maybe [String]
diamond char = Just . (++) pyramid $ (reverse . init) pyramid
    where
        list = ['A'..(toUpper char)]
        numOfChar = length list
        pyramid = fstTriple $ foldr foldDiamond ([], 0, numOfChar - 1) list
        
foldDiamond :: Char -> ([[Char]], Int, Int) -> ([[Char]], Int, Int)
foldDiamond char (xs, num, chars)
    | chars /= num = ( (outerPadding ++ [char] ++ innerPadding ++ [char] ++ outerPadding) : xs , num + 1, chars)
    | otherwise = ( (outerPadding ++ [char] ++ outerPadding) : xs , num, chars)
    where
        outerPadding = take num (repeat ' ')
        innerPadding = take (((chars - num) * 2) - 1) (repeat ' ')

fstTriple :: (a,b,c) -> a
fstTriple (a,_,_) = a
