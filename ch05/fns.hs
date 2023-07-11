module Fns where

i :: a -> a
i = id

c :: a -> b -> a
c x _  = x

c'' :: b -> a -> b
c'' = c

r :: [a] -> [a]
r = id

r' :: [a] -> [a]
r' = tail

co :: (b -> c) -> (a -> b) -> a -> c
co = (.)

a :: (a -> c) -> a -> a
a _ x = x

a' :: (a -> b) -> a -> b
a' = ($)

munge :: (x -> y)
      -> (y -> (w, z))
      -> x
      -> w
munge f g x = fst $ g $ f x
