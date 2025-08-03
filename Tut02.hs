module Tut02 where

import Prelude hiding (filter, foldl, map, scanl, zip, zipWith)

-- Per Hand
map :: (t -> a) -> [t] -> [a]
map f [] = []
map f (x : xs) = f x : map f xs

-- Mit Fold
map' :: (Foldable t1) => (t2 -> a) -> t1 t2 -> [a]
map' f l = foldr (\e o -> f e : o) [] l

map'' :: (Foldable t1) => (t2 -> a) -> t1 t2 -> [a]
map'' f = foldr (\e o -> f e : o) []

-- Per Hand
filter :: (a -> Bool) -> [a] -> [a]
filter f [] = []
filter f (x : xs)
  | f x = x : filter f xs
  | otherwise = filter f xs

-- Mit Fold
filter' :: (Foldable t) => (a -> Bool) -> t a -> [a]
filter' pred l = foldr (\x filtered -> if pred x then x : filtered else filtered) [] l

squares :: [Integer] -> [Integer]
squares = map (^ 2)

zip :: [a] -> [b] -> [(a, b)]
zip [] _ = []
zip _ [] = []
zip (a : as) (b : bs) = (a, b) : zip as bs

zipWith :: (t1 -> t2 -> a) -> [t1] -> [t2] -> [a]
zipWith f (a : as) (b : bs) = f a b : zipWith f as bs
zipWith f as bs = []

zipWith' :: (t1 -> t2 -> a) -> [t1] -> [t2] -> [a]
zipWith' f as bs = map (\(x, y) -> f x y) (zip as bs)

zipWith'' :: (a1 -> b -> a2) -> [a1] -> [b] -> [a2]
zipWith'' f as bs = map (uncurry f) (zip as bs)

foldl :: (t1 -> t2 -> t1) -> t1 -> [t2] -> t1
foldl f o [] = o
foldl f o (p : ps) = foldl f (f o p) ps

scanl :: (t1 -> t2 -> t1) -> t1 -> [t2] -> [t1]
scanl f i [] = [i]
scanl f i (x : l) = i : scanl f (f i x) l

natTuple :: [(Integer, Integer)]
natTuple = [(x, t - x) | t <- [2 ..], x <- [1 .. (t - 1)]]