{-# LANGUAGE ExistentialQuantification #-}

module Existential where

import Control.Exception (ArithException(..), AsyncException(..))
import Data.Monoid ((<>), First(..))
import Data.Maybe (fromMaybe)
import Data.Typeable

-- can also do this with a GADT
data MyException = forall e . (Show e, Typeable e) => MyException e

-- e.g. (needs LANGUAGE GADTs)
{--
data MyException where
  MyException :: (Show e, Typeable e) => e -> MyException
--}

instance Show MyException where
  showsPrec p (MyException e) = showsPrec p e

multiError :: Int -> Either MyException Int
multiError 0 = Left $ MyException DivideByZero
multiError 1 = Left $ MyException StackOverflow
multiError n = Right n

data SomeError = Arith ArithException
               | Async AsyncException
               | SomethingElse
               deriving Show

discriminateError :: MyException -> SomeError
discriminateError (MyException e) = fromMaybe SomethingElse $
  getFirst $ First (Arith <$> cast e) <> First (Async <$> cast e)
