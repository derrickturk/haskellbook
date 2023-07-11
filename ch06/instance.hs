data Identity a = Identity a

instance Eq a => Eq (Identity a) where
  (Identity x) == (Identity y) = x == y

data StringOrInt =
    TisAnInt Int
  | TisAString String

instance Eq StringOrInt where
  TisAnInt x == TisAnInt y = x == y
  TisAString x == TisAString y = x == y
  _ == _ = False
