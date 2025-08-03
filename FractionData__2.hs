module FractionData where

data Fraction = Fraction Integer Integer

instance Num Fraction where
  (+) :: Fraction -> Fraction -> Fraction
  (+) a b = _

  (*) :: Fraction -> Fraction -> Fraction
  (*) a b = _

  abs :: Fraction -> Fraction
  abs a = _

  signum :: Fraction -> Fraction
  (signum) a = _

  fromInteger :: Integer -> Fraction
  fromInteger a = _

  negate :: Fraction -> Fraction
  negate a = _

instance Eq Fraction where
  (==) :: Fraction -> Fraction -> Bool
  (==) a b = _

instance Ord Fraction where
  (<=) :: Fraction -> Fraction -> Bool
  (<=) a b = _