module DiffList (
    DList
  , toList
  , empty
  , singleton
  , append
  , cons
  , snoc
) where

newtype DList a = DL { unDL :: [a] -> [a] }

toList :: DList a -> [a]
{-# INLINE toList #-}
toList = ($ []) . unDL

empty :: DList a
{-# INLINE empty #-}
empty = DL $ id

singleton :: a -> DList a
{-# INLINE singleton #-}
singleton = DL . (:)

append :: DList a -> DList a -> DList a
{-# INLINE append #-}
append xs ys = DL $ unDL xs . unDL ys

infixr `cons`
cons :: a -> DList a -> DList a
{-# INLINE cons #-}
cons x xs = DL $ (x:) . unDL xs

infixl `snoc`
snoc :: DList a -> a -> DList a
{-# INLINE snoc #-}
snoc xs x = DL $ (unDL xs) . (x:)
