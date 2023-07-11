module Database where

import Data.Time
import Data.List (foldl')

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
            (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr
  (\d ts -> case d of
      DbDate t -> t:ts
      _ -> ts)
  []

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr
  (\d ns -> case d of
      DbNumber n -> n:ns
      _ -> ns)
  []

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb db = let nums = filterDbNumber db in
  (sum $ map fromIntegral $ nums) / (fromIntegral $ length nums)

-- it occurs to me we might have been supposed to use a fold to do this
-- it might as well be strict because, you know, average
avgDb' :: [DatabaseItem] -> Double
avgDb' db = total / (fromIntegral len) where
  (total, len) = foldl' f (0.0, 0) db
  f (t, l) (DbNumber n) = (t + fromIntegral n, l + 1)
  f (t, l) _ = (t, l)
