module Chars where

import Data.Char

caps :: [Char] -> [Char]
caps = fmap toUpper

rev :: [Char] -> [Char]
rev = reverse

composed :: [Char] -> [Char]
composed = caps . rev

fmapped :: [Char] -> [Char]
fmapped = caps <$> rev

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> caps <*> rev

tupled' :: [Char] -> ([Char], [Char])
tupled' = do
  x <- caps
  y <- rev
  return (x, y)

tupled'' :: [Char] -> ([Char], [Char])
tupled'' = caps >>= \c -> rev >>= \r -> return (c, r)
