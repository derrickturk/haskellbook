import Data.Monoid

class Monoid' a where
  mempty' :: a
  mappend' :: a -> a -> a

instance Monoid' [a] where
  mempty' = []
  mappend' = (++)

{--
instance Num a => Monoid' a where
  mempty' = 0
  mappend' = (+)
--}

newtype MonoidProduct a = MonoidProduct { unMonoidProduct :: a }

instance Num a => Monoid' (MonoidProduct a) where
  mempty' = MonoidProduct 1
  mappend' x y = MonoidProduct $ unMonoidProduct x * unMonoidProduct y

data Optional a = Nada | Only a deriving (Eq, Show)

-- "real" Monoid, fake Maybe
instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend Nada Nada = Nada
  mappend x y = Only $ project x `mappend` project y where
    project Nada = mempty
    project (Only x) = x

-- another Maybe Monoid
newtype Firstish a = Firstish { getFirstish :: Optional a }
  deriving (Eq, Show)

instance Monoid (Firstish a) where
  mempty = Firstish Nada
  mappend (Firstish Nada) (Firstish Nada) = Firstish Nada
  mappend fx@(Firstish (Only x)) _ = fx
  mappend (Firstish Nada) fy@(Firstish (Only y)) = fy

newtype Mem s a = Mem { runMem :: s -> (a, s) }

instance Monoid a => Monoid (Mem s a) where
  mappend m1 m2 = Mem f where
    f s0 = let (a1, s1) = runMem m1 s0 in
             let (a2, s2) = runMem m2 s1 in
               (a1 `mappend` a2, s2)
  mempty = Mem $ \s -> (mempty, s)

f' = Mem $ \s -> ("hi", s + 1)

main = do
  let rmzero = runMem mempty 0
      rmleft = runMem (f' <> mempty) 0
      rmright = runMem (mempty <> f') 0
  print $ rmleft
  print $ rmright
  print $ (rmzero :: (String, Int))
  print $ rmleft == runMem f' 0
  print $ rmright == runMem f' 0
