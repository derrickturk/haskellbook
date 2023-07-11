module Vignere (vignere, unVignere) where

import Data.Char

shiftChar :: Int -> Char -> Char
shiftChar shift c
  | isAsciiLower c = chr $ ord 'a' + (ord c - ord 'a' + shift) `mod` 26
  | isAsciiUpper c = chr $ ord 'A' + (ord c - ord 'A' + shift) `mod` 26
  | otherwise = c

caesar :: Int -> String -> String
caesar = map . shiftChar

unCaesar :: Int -> String -> String
unCaesar = map . shiftChar . negate

-- ok according to the book you skip the key over spaces but ehhh
type Keyword = String

shiftFor :: Char -> Int
shiftFor k
  | isAsciiLower k = ord k - ord 'a'
  | isAsciiUpper k = ord k - ord 'A'
  | otherwise = 0

vignere :: Keyword -> String -> String
vignere k s = zipWith f (cycle k) s where
  f k c = shiftChar (shiftFor k) c

unVignere :: Keyword -> String -> String
unVignere k s = zipWith f (cycle k) s where
  f k c = shiftChar (- shiftFor k) c
