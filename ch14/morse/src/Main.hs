module Main where

import Morse (stringToMorse, morseToChar)
import Data.List (intercalate)
import Data.Traversable (traverse)
import Control.Monad (forever, when)
import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
import System.IO (hGetLine, hIsEOF, stdin)

convert :: (String -> Maybe String) -> IO ()
convert f = forever $ do
  eof <- hIsEOF stdin
  when eof exitSuccess
  line <- hGetLine stdin
  case f line of
    Just s -> putStrLn s
    Nothing -> (putStrLn $ "ERROR: " ++ line) >> exitFailure

convertToMorse :: IO ()
convertToMorse = convert toMorse

convertFromMorse :: IO ()
convertFromMorse = convert fromMorse

toMorse :: String -> Maybe String
toMorse = (fmap $ intercalate " ") . stringToMorse

fromMorse :: String -> Maybe String
fromMorse = traverse morseToChar . words

main :: IO ()
main = do
  mode <- getArgs
  case mode of
    [arg] -> case arg of
      "from" -> convertFromMorse
      "to" -> convertToMorse
      _ -> usageMsg
    _ -> usageMsg
  where usageMsg = putStrLn "Usage: morse [from|to]" >> exitFailure
