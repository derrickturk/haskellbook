module Cipher where

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
