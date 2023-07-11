{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany = (> 42)

newtype Something = Something Int 

instance TooMany Something where
  tooMany (Something n) = n > 11

-- this requires GeneralizedNewtypeDeriving: now we can use the
--   Int instance automatically for newtypes wrapping Int
newtype SomethingElse = SomethingElse Int
  deriving TooMany

{-- OK, without FlexibleInstances we can do this:
instance (TooMany a, TooMany b) => TooMany (a, b) where
  tooMany (x, y) = tooMany x || tooMany y
--}

{-- or this:
newtype IntString = IntString (Int, String)
instance TooMany IntString where
  tooMany (IntString (n, s)) = tooMany n || s > "hello"
--}

{-- but not this (we'd need {-# LANGUAGE FlexibleInstances #-}):
instance TooMany (Int, String) where
  tooMany (n, s) = tooMany n || s > "hello"
--}
