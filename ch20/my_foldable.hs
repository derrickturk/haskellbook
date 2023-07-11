-- given foldr:
foldMap' :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
foldMap' f xs = foldr (mappend . f) mempty xs

-- given foldMap
fold' :: (Monoid m, Foldable t) => t m -> m
fold' = foldMap' id
-- fold' = foldr' mappend mempty

-- ok, this is a hair obscure, I had to cheat and read the source:
-- we create a Monoid instance for "endomorphisms" i.e. (a -> a)
newtype Endo' a = Endo' { unEndo' :: a -> a }
instance Monoid (Endo' a) where
  mempty = Endo' id
  mappend g f = Endo' (unEndo' g . unEndo' f)

-- and then our f (a -> b -> b) is used to make an (a -> Endo' b)
-- this is basically "fucking witchcraft"
foldr' :: Foldable t => (a -> b -> b) -> b -> t a -> b
foldr' f z xs = unEndo' (foldMap (Endo' . f) xs) z
