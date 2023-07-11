module MonadTrans where

newtype IdentityT m a = IdentityT { runIdentityT :: m a }
  deriving (Eq, Show)

instance Functor m => Functor (IdentityT m) where
  fmap f (IdentityT x) = IdentityT $ f <$> x

instance Applicative m => Applicative (IdentityT m) where
  pure = IdentityT . pure
  (IdentityT f) <*> (IdentityT x) = IdentityT $ f <*> x

instance Monad m => Monad (IdentityT m) where
  (IdentityT m) >>= k = IdentityT $ m >>= runIdentityT . k
