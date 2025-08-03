module Blatt0 where

-- mit if then else
max3' a b c =
  if a >= b
    then (if a >= c then a else c)
    else (if b >= c then b else c)

-- mit guard notation
max3'' a b c
  | a >= b && a >= c = a
  | b >= c = b
  | otherwise = c

-- mit max funktion
max3''' a b c = max a (max b c)