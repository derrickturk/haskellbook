{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Data.IORef
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import Web.Scotty.Trans

data Config = Config {
    counts :: IORef (M.Map Text Integer)
  , prefix :: Text
  }

type Scotty = ScottyT Text (ReaderT Config IO)
type Handler = ActionT Text (ReaderT Config IO)

updateCount :: Text -> M.Map Text Integer -> (M.Map Text Integer, Integer)
updateCount k m = let c = (M.findWithDefault 0 k m) + 1 in (M.insert k c m, c)

app :: Scotty ()
app = get "/:key" $ do
  key <- mappend <$> (lift $ reader prefix) <*> param "key"
  countsRef <- lift $ reader counts
  counts <- liftIO $ readIORef $ countsRef

  let (newCounts, newInteger) = updateCount key counts

  liftIO $ writeIORef countsRef newCounts
  html $ mconcat [ "<h1>Success! Count was: "
                 , TL.pack $ show newInteger
                 , "</h1>"
                 ]

main :: IO ()
main = do
  [prefixArg] <- getArgs
  counter <- newIORef M.empty
  let config = Config counter (TL.pack prefixArg)
      runR r = runReaderT r config
  scottyT 3000 runR app
