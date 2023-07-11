module Main where

import Hello
import Dogs
import System.IO (hSetBuffering, stdout, BufferMode(..))

main :: IO ()
main = 
  hSetBuffering stdout NoBuffering >>
  putStr "Name: " >>
  getLine >>=
  sayHello >>
  dogs
