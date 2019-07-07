module TwelveDays (recite) where

type Order = String
type Present = String

data Lyric = Lyric Order Present

songLyrics :: [Lyric]
songLyrics = [ Lyric "first" "a Partridge in a Pear Tree."
             , Lyric "second" "two Turtle Doves, and a Partridge in a Pear Tree."
             , Lyric "third" "three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
             , Lyric "fourth" "four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
             , Lyric "fifth" "five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
             , Lyric "sixth" "six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
             , Lyric "seventh" "seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
             , Lyric "eighth" "eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
             , Lyric "ninth" "nine Ladies Dancing, eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
             , Lyric "tenth" "ten Lords-a-Leaping, nine Ladies Dancing, eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
             , Lyric "eleventh" "eleven Pipers Piping, ten Lords-a-Leaping, nine Ladies Dancing, eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
             , Lyric "twelfth" "twelve Drummers Drumming, eleven Pipers Piping, ten Lords-a-Leaping, nine Ladies Dancing, eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
             ]

    

recite :: Int -> Int -> [String]
recite start stop = (map lyricToString) . (take (stop - start')) . (drop start') $ songLyrics
    where 
        lyricToString (Lyric order present) =  "On the " 
                                            ++ order 
                                            ++ " day of Christmas my true love gave to me: "
                                            ++ present
        start' = start - 1