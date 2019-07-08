module LinkedList
    ( LinkedList
    , datum
    , fromList
    , isNil
    , new
    , next
    , nil
    , reverseLinkedList
    , toList
    ) where

data LinkedList a = Nil | a :. LinkedList a deriving (Eq, Show)

infixr 5 :.

datum :: LinkedList a -> a
datum (x:.xs) = x

fromList :: [a] -> LinkedList a
fromList = foldr go Nil 
        where 
            go item linkedList = item :. linkedList

isNil :: LinkedList a -> Bool
isNil Nil = True
isNil _   = False

new :: a -> LinkedList a -> LinkedList a
new = (:.)

next :: LinkedList a -> LinkedList a
next Nil = Nil
next (x:.xs) = xs

nil :: LinkedList a
nil = Nil

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList = go Nil 
            where
                go rev Nil      = rev
                go rev (x:.xs)  = go (x :. rev) xs

toList :: LinkedList a -> [a]
toList l = reverse . (go []) $ l
    where
        go list Nil     = list
        go list (x:.xs) = go (x:list) xs
