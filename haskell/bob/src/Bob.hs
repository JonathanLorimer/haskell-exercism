module Bob (responseFor) where
import              Data.Char (isSpace, isUpper, isAlpha)


responseFor :: String -> String
responseFor xs
    | allSpace = "Fine. Be that way!"
    | question && anger =  "Calm down, I know what I'm doing!"
    | question = "Sure."
    | anger = "Whoa, chill out!"
    | otherwise = "Whatever."
        where
            noSpace = filter (not . isSpace) $ xs 
            question = (== '?') . last $ noSpace
            letters = filter (isAlpha) xs
            allSpace = all isSpace xs
            anger = (length letters) > 0 && all isUpper letters
