{-# LANGUAGE GeneralizedNewtypeDeriving #-}

newtype Either' a b = Either' { unE' :: Either a b }
  deriving (Eq, Show, Functor, Applicative, Monad, Foldable)

instance Traversable (Either' a) where
  sequenceA (Either' (Left x)) = pure (Either' $ Left x)
  sequenceA (Either' (Right x)) = fmap (Either' . Right) x
