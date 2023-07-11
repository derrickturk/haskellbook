module TupleFunctions where

add2 :: Num a => (a, a) -> a
add2 (x, y) = x + y

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (w, _, x) (y, _, z) = ((w, y), (x, z))
