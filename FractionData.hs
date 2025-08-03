module FractionData where

data Fraction = Fraction Integer Integer

instance Num Fraction where
  (+) :: Fraction -> Fraction -> Fraction
  (+) (Fraction p1 q1) (Fraction p2 q2) =
    Fraction (p1 * q2 + p2 * q1) (q1 * q2)

  (*) :: Fraction -> Fraction -> Fraction
  (*) (Fraction a b) (Fraction x y) = Fraction (a * x) (b * y)

  abs :: Fraction -> Fraction
  abs (Fraction p1 q1) = Fraction (abs p1) (abs q1)

  signum :: Fraction -> Fraction
  signum (Fraction p q) = fromInteger (signum p * signum q)

  fromInteger :: Integer -> Fraction
  fromInteger a = Fraction a 1

  negate :: Fraction -> Fraction
  negate (Fraction p q) = Fraction (-p) q

instance Eq Fraction where
  (==) :: Fraction -> Fraction -> Bool
  (==) (Fraction a b) (Fraction c d) = (a * d) == (c * b)

instance Ord Fraction where
  (<=) :: Fraction -> Fraction -> Bool
  (<=) (Fraction p1 q1) (Fraction p2 q2) =
    signum q1 * p1 * abs q2 <= signum q2 * p2 * abs q1