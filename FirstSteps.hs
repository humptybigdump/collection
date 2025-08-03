{-
--Auswertung mit HLS Eval Plugin
>>>double 4
8
>>>double 16 == 32
True
-}
module FirstSteps where

double :: Num a => a -> a
double x = 2 * x

dSum :: Num a => a -> (a -> a)
dSum x y = double x + double y

sum3 :: Num a => a -> a -> a -> a
sum3 a b c = a + b + c

sum4 :: Num a => a -> a -> a -> a -> a
sum4 a b c d = sum3 a b c + d
