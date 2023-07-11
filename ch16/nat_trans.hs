{-# LANGUAGE RankNTypes #-}

-- a type to ensure a transformation only changes structure,
--   not contents: we "hide" the functor's inner type behind a forall
type Nat f g = forall a . f a -> g a

maybeToList :: Nat Maybe []
maybeToList (Just x) = [x]
maybeToList Nothing = []

-- you can't specify (Num a) because a is "invisible" in the type
maybeToLis :: Nat Maybe []
maybeToLis (Just x) = [x + 1]
maybeToLis Nothing = []
