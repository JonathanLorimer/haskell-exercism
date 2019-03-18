module Clock (addDelta, fromHourMin, toString) where

-- import qualified Prelude as P

type Hour = Int
type Minute = Int
data Clock = Clock Hour Minute

instance Eq Clock where
    Clock h1 m1 == Clock h2 m2 = h1 == h2 && m1 == m2

mod24 :: Int -> Int
mod24 = flip mod 24

divMod60 :: Int -> (Int, Int)
divMod60 = flip divMod 60

fromHourMin :: Int -> Int -> Clock
fromHourMin hour mins = Clock boundedHours $ snd boundedMins
    where
        boundedMins = divMod60 mins
        boundedHours = mod24 $ hour + fst boundedMins

toString :: Clock -> String
toString (Clock hour mins) = formattedHour ++ ":" ++ formattedMin
    where
        formattedHour   = if hour < 10 
                          then "0" ++ (show hour)
                          else show hour
        formattedMin    = if mins < 10 
                          then "0" ++ (show mins)
                          else show mins

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour mins (Clock hour' mins') = Clock combinedHours $ snd combinedMins
            where
                combinedMins = divMod60 $ mins + mins'
                combinedHours = mod24 $ hour + hour' + fst combinedMins

                
