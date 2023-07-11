module ListyInstances where

import Data.Monoid
import Listy

instance Monoid (Listy a) where
  mempty = Listy mempty
  mappend (Listy x) (Listy y) = Listy $ mappend x y
