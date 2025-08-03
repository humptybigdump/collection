-- Haskell Kombinatoren

intermsl :: (a -> b -> a) -> a -> [b] -> [a]
intermsl f i [] = [i]
intermsl f i (x:xs) = i : intermsl f (f i x) xs

euler :: (Double -> Double -> Double) -> Double -> Double -> Double -> [Double]
euler f x0 y0 h = yns
  where
    -- xn = x0 + n*h = xn-1 + h
    xns  = iterate (+h) x0
    yns  = y0 : zipWith (+) yns (map (*h) yn's)
    yn's = zipWith f xns yns

-- Haskell Datenstrukturen

data Tree a = Node a [Tree a]
  deriving Show

treeIndex :: Tree a -> [Int] -> Tree a
treeIndex t [] = t
treeIndex (Node a ts) (p:ps)
  | p < 0 || p >= length ts = error "Ungültiger Index"
  | otherwise = treeIndex (ts !! p) ps

treePositions :: Tree a -> [[Int]]
treePositions (Node a ts) = [] : [i : path | i    <- [0..(length ts - 1)], 
                                             path <- treePositions (ts !! i)]

changeTree :: (Tree a -> Tree a) -> [Int] -> Tree a -> Tree a
changeTree f [] t = f t
changeTree f (p:ps) (Node a ts)
  | p < 0 || p >= length ts = Node a ts
  | otherwise = Node a (prev ++ changeTree f ps (ts !! p) : after)
  where
    prev  = take p ts
    after = drop (p+1) ts

overrideTree :: Tree a -> [Int] -> Tree a -> Tree a
overrideTree t' ps t = changeTree (const t') ps t