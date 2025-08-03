module Tut01 where

fac n = facAcc 1 n
  where
    facAcc acc 1 = acc
    facAcc acc x = facAcc (acc * x) (x - 1)

-- Implementierung mit tail recursion
fib n = fibAcc n 0 1
  where
    fibAcc 0 x y = x
    fibAcc 1 x y = y
    fibAcc n x y = fibAcc (n - 1) y (x + y)

-- Implementierung ohne tail recursion
fib' 0 = 0
fib' 1 = 1
fib' n = fib (n - 1) + fib (n - 2)

-- Implementierung, welche die Berechnung der vorherigen Fib Zahlen wiederverwendet
fibs 0 = []
fibs 1 = [0]
fibs 2 = [0, 1]
fibs n = fibsAcc [1, 0] (n - 2)
  where
    fibsAcc l 0 = reverse l
    fibsAcc l n = fibsAcc ((head l + l !! 1) : l) (n - 1)

-- Naive Implementierung mit Map ohne Unterversorgung
fibs' n = map (\x -> fib x) [0 .. (n - 1)]

-- Naive Implementierung mit Map mit Unterversorgung
fibs'' n = map fib [0 .. n - 1]

-- Implementierung mit tail recursion
productL l = productLAcc 1 l
  where
    productLAcc acc [] = acc
    productLAcc acc (x : xs) = productLAcc (acc * x) xs

-- Implementierung ohne tail recursion
productL' [] = 1
productL' (x : xs) = x * productL' xs

odds = filter odd

evens = filter even

squares = map (^ 2)
