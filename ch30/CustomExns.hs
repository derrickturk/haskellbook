module CustomExns where

import Control.Exception

data NotDivThree = NotDivThree Int
  deriving (Eq, Show)

instance Exception NotDivThree

data NotEven = NotEven Int
  deriving (Eq, Show)

instance Exception NotEven

evenAndThreeDiv :: Int -> IO Int
evenAndThreeDiv i
  | rem i 3 /= 0 = throwIO $ NotDivThree i
  | odd i = throwIO $ NotEven i
  | otherwise = return i

withDefaults :: IO Int -> IO Int
withDefaults = flip catches
  [ Handler $ \(NotEven x) -> return $ x * 2
  , Handler $ \(NotDivThree x) -> return $ x * 3
  ]
