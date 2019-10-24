{-# LANGUAGE RankNTypes #-}

module Matrix (saddlePoints) where

import Data.Array (Array, assocs, bounds, elems)
import Data.Ix (Ix, range)
import Data.Bifunctor (bimap)
import Data.List (maximumBy, minimumBy, null)

type Matrix i e = Array (i, i) e

saddlePoints :: (Ord e, Ix i) => Matrix i e -> [(i,i)]
saddlePoints m
  | null . elems $ m = []
  | otherwise =
    let b = bounds m
        rIxs = range . bimap fst fst $ b
        cIxs = range . bimap snd snd $ b
        rGet = flip getRow m
        cGet = flip getColumn m
        rMax = concatMap (maximumsBy . rGet) rIxs
        cMin = concatMap (minimumsBy . cGet) cIxs
     in fmap fst . filter (`elem` rMax) $ cMin

getRow :: (Ix i, Eq i) => i -> Matrix i e -> [((i, i), e)]
getRow ix = foldr go [] . assocs
  where
    go a@((r,_), _) z = if r == ix
                         then a : z
                         else z

getColumn :: (Ix i, Eq i) => i -> Matrix i e -> [((i, i), e)]
getColumn ix = foldr go [] . assocs
  where
    go a@((_,c), _) z = if c == ix
                        then a : z
                        else z

compSnd :: Ord a => (c, a) -> (b, a) -> Ordering
compSnd (_, a) (_, b) = compare a b

msBy :: Ord e
     => (((fst, e) -> (fst, e) -> Ordering) -> [(fst, e)] -> (fst, e))
     -> [(fst, e)]
     -> [(fst, e)]
msBy mBy a =
  let mval = snd $ mBy compSnd a
  in filter ((== mval) . snd) a

minimumsBy :: Ord e => [(fst, e)] -> [(fst, e)]
minimumsBy = msBy minimumBy

maximumsBy :: Ord e => [(fst, e)] -> [(fst, e)]
maximumsBy = msBy maximumBy
