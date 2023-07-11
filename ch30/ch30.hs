module Ch30 where

import Control.Exception
import Data.Typeable

handler :: SomeException -> IO ()
handler (SomeException e) = do
  print (typeOf e)
  putStrLn $ "error: " ++ show e

main = do
  (readFile "potato" >>= putStr) `catch` handler
  x <- try $ return (17 `div` 0) :: IO (Either ArithException Int)
  print x
