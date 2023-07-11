module Main where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader

embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = return 1

-- the innermost monad structurally is the outermost transformer, lexically

maybeUnwrap :: ExceptT String (ReaderT () IO) (Maybe Int)
maybeUnwrap = runMaybeT embedded

eitherUnwrap :: ReaderT () IO (Either String (Maybe Int))
eitherUnwrap = runExceptT maybeUnwrap

readerUnwrap :: IO (Either String (Maybe Int))
readerUnwrap = runReaderT eitherUnwrap ()

-- well, this is dumb
embedded' :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded' = MaybeT $ ExceptT $ ReaderT $ const $ return $ Right $ Just 1

main :: IO ()
main = do
  putStrLn "hello world"
