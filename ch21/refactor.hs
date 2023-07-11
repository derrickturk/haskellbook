module Refactor where

import Control.Monad ((>=>))

data Query = Query
data SomeObj = SomeObj
data IoOnlyObj = IoOnlyObj
data Err = Err

decodeFn :: String -> Either Err SomeObj
decodeFn = undefined

fetchFn :: Query -> IO [String]
fetchFn = undefined

makeIoOnlyObj :: [SomeObj] -> IO [(SomeObj, IoOnlyObj)]
makeIoOnlyObj = undefined

pipelineFn :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn query = do
  a <- fetchFn query
  case sequence (map decodeFn a) of
    (Left err) -> return $ Left $ err
    (Right res) -> do
      a <- makeIoOnlyObj res
      return $ Right a

-- better:
pipelineFn' :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn' query = do
  a <- fetchFn query -- a :: String
  objs <- return $ traverse decodeFn a -- objs :: Either Err [SomeObj]
  traverse makeIoOnlyObj objs

-- even better:
pipelineFn'' :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn'' = fetchFn >=> (traverse makeIoOnlyObj) . (traverse decodeFn)
