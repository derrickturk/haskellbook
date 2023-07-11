{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Data.Monoid ((<>))
import Control.Monad.Trans.Class

main :: IO ()
main = scotty 3000 $ get "/:word" $ do
  verb <- param "word"
  lift $ putStrLn "hello"
  html $ "<h1>" <> verb <> " my shit up</h1>"
