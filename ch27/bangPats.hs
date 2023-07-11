{-# LANGUAGE BangPatterns #-}

module BangPats where

data NoBang = NoBang Int String

-- strict data constructors don't actually require
--   BangPatterns
data BangBang = BangBang !Int !String

dontPeek :: Bool -> Int
dontPeek _ = 1

doPeek :: Bool -> Int
doPeek = (`seq` 1)

doPeekBang :: Bool -> Int
doPeekBang !b = 1

notBad :: Int
notBad = NoBang 1 undefined `seq` 17

bad :: Int
bad = BangBang 1 undefined `seq` 17
