module Examples where

import Control.Applicative (liftA2)

newtype Identity a = Identity { runIdentity :: a }
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity x) = Identity $ f x

newtype Compose f g a = Compose { getCompose :: f (g a) }
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  -- :: (a -> b) -> Compose f g a -> Compose f g b
  fmap f (Compose x) = Compose $ (fmap . fmap) f x

-- fuck this instance in particular
instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure = Compose . (pure . pure)
  (Compose fgf) <*> (Compose fgx) = Compose $ (<*>) <$> fgf <*> fgx

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap f (Compose c) = foldMap (foldMap f) c

instance (Traversable f, Traversable g) => Traversable (Compose f g) where 
  traverse f (Compose c) = Compose <$> traverse (traverse f) c
