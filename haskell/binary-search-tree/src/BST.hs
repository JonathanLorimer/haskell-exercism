module BST
    ( BST
    , bstLeft
    , bstRight
    , bstValue
    , empty
    , fromList
    , insert
    , singleton
    , toList
    ) where

data BST a = Node a (BST a) (BST a)
           | Empty
           deriving (Eq, Show)

bstLeft :: BST a -> Maybe (BST a)
bstLeft Empty        = Nothing
bstLeft (Node _ l _) = Just l

bstRight :: BST a -> Maybe (BST a)
bstRight Empty        = Nothing
bstRight (Node _ _ r) = Just r

bstValue :: BST a -> Maybe a
bstValue Empty        = Nothing
bstValue (Node a _ _) = Just a

empty :: BST a
empty = Empty

fromList :: Ord a => [a] -> BST a
fromList = foldl (flip insert) empty

insert :: Ord a => a -> BST a -> BST a
insert a Empty        = singleton a
insert a (Node b l r) =
  if a > b
     then Node b l (insert a r)
     else Node b (insert a l) r

singleton :: a -> BST a
singleton a = Node a empty empty

toList :: BST a -> [a]
toList Empty = []
toList (Node a l r) = toList l <> [a] <> toList r
