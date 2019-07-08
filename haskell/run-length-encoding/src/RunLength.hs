module RunLength (decode, encode) where

import Data.Char (isDigit)

encode :: String -> String
encode str = case foldr go (0, []) str of
                (0, encodedStr) -> encodedStr
                (count, xs) -> (show (count + 1)) ++ xs
            where
                go cur (0, []) = (0, [cur])
                go cur (count, acc@(prev:xs))
                    | cur == prev   = (count + 1, acc)
                    | count > 0     = (0, cur : ((show (count + 1)) ++ acc))
                    | otherwise     = (0, cur : acc)

decode :: String -> String
decode str = case foldr go ("", []) str of
                ("", decodedStr) -> decodedStr
                (count, (x:xs)) -> (replicate (read count) x) ++ xs
            where
                go cur ("", []) = ("", [cur])
                go cur (count, acc@(prev:xs))
                    | isDigit cur   = (cur:count, acc)
                    | count /= ""   = ("", cur : ((replicate (read count) prev) ++ xs))
                    | otherwise     = ("", cur : acc)
