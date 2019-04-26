module SecretHandshake (handshake) where

handshake :: Int -> [String]
handshake = reverseOrNot . decode . reverse . toBinary


toBinary :: Int -> [ Int ]
toBinary 0 = [ 0 ]
toBinary 1 = [ 1 ]
toBinary n = toBinary ( n `quot` 2 ) ++ [ n `rem` 2 ]

decode :: [Int] -> [String]
decode = (map fst) . (filter ((== 1) . snd)) . (zip act)
    where
        act = [ "wink"
              , "double blink"
              , "close your eyes"
              , "jump"
              , "reverse"
              ]

reverseOrNot :: [String] -> [String]
reverseOrNot [] = []
reverseOrNot xs
        | last xs == "reverse" = reverse . init $ xs
        | otherwise = xs