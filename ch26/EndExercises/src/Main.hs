module Main where

import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Data.Functor.Identity

rDec :: Num a => Reader a a
rDec = reader (+ 1)

rShow :: Show a => ReaderT a Identity String
rShow = reader show

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = do
  x <- ask
  lift $ putStrLn $ "Hi: " ++ show x
  return $ x + 1

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = do
  x <- get
  lift $ putStrLn $ "Hi: " ++ show x
  put $ x + 1
  return $ show x

isValid :: String -> Bool
isValid = elem '!'

maybeExcite :: MaybeT IO String
maybeExcite = do
  v <- lift getLine
  guard $ isValid v
  return v

doExcite :: IO ()
doExcite = do
  putStrLn "say something exciting"
  excite <- runMaybeT maybeExcite
  case excite of
    Nothing -> putStrLn "more exciting!"
    Just e -> putStrLn ("that was exciting: " ++ e)

main :: IO ()
main = do
  putStrLn "hello world"
