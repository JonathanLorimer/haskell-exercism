module WordCount (wordCount) where

import Control.Arrow
import Data.List
import Data.Char

wordCount :: String -> [(String, Int)]
wordCount =
  fmap (head &&& length) . group . sort . strip . fmap toLower

strip :: String -> [String]
strip s =
  let p c = isAlphaNum c || c == '\''
  in case dropWhile (not . isAlphaNum) s of
        ""      -> []
        trimmed -> if last w == '\''
                      then init w : strip rest
                      else w : strip rest
            where (w, rest) = span p trimmed
