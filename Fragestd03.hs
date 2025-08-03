module Fragestd03 where

splitWhen :: (t -> Bool) -> [t] -> ([t], [t])
splitWhen _ [] = ([], [])
splitWhen predicate (x : xs)
  | predicate x = ([], x : xs)
  | otherwise = let (first, last) = splitWhen predicate xs in (x : first, last)

splitWhen'' :: (a -> Bool) -> [a] -> ([a], [a])
splitWhen'' f l = (takeWhile (not . f) l, dropWhile (not . f) l)

group :: (Eq a) => [a] -> [[a]]
group [] = []
group (x : xs) = fstSplitted : group sndSplitted
  where
    (fstSplitted, sndSplitted) = splitWhen (/= x) (x : xs)

encode :: (Eq a) => [a] -> [(a, Int)]
encode l = map (\g -> (head g, length g)) (group l)

encode' :: Eq a => [a] -> [(a, Int)]
encode' l = [(head x, length x) | x <- group l]

decode :: [(a, Int)] -> [a]
decode [] = []
decode (x : xs)
  | snd x > 0 = fst x : decode ((fst x, snd x - 1) : xs)
  | otherwise = decode xs

decode'' :: [(a, Int)] -> [a]
decode'' = concatMap (\(x, n) -> replicate n x)

decode''' :: [(a, Int)] -> [a]
decode''' = concatMap . uncurry . flip $ replicate

decode'''' :: [(a, Int)] -> [a]
decode'''' [] = []
decode'''' ((a, n) : xs) = let as = a : as in take n as ++ decode'''' xs

hamming :: [Integer]
hamming = 1 : merge (map (2 *) hamming) (merge (map (3 *) hamming) (map (5 *) hamming))

hamming' :: [Integer]
hamming' = [2 ^ i * 3 ^ j * 5 ^ (n - i - j) | n <- [0 ..], i <- [0 .. n], j <- [0 .. n - i]]

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge xs@(x : xt) ys@(y : yt)
  | x == y = x : merge xt yt
  | x < y = x : merge xt ys
  | otherwise = y : merge xs yt