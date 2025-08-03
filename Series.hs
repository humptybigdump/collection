module Series where

--Implementation von funcSum ohne tail recusion
funcSum f [] = 0
funcSum f (x:xs) = f x + funcSum f xs

--Implementation von funcSum mit tail recursion
funcSum' f l = funcSumAcc 0 f l
    where 
        funcSumAcc acc f [] = acc
        funcSumAcc acc f (x:xs) = funcSumAcc (acc + f x) f xs

--Implementierung ohne Unterversorgung
squareSum :: Num a => [a] -> a
squareSum l = funcSum (\y -> y * y) l

--Implementierung mit Unterversorgung
squareSum' :: Num a => [a] -> a
squareSum' = funcSum (^2)

cubeSum l = funcSum (\y -> y * y * y) l

mysterySum l = funcSum (\y -> 1 / ((4*y+1)*(4*y+3))) l
