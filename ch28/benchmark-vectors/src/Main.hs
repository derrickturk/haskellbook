module Main where

import Criterion.Main

import Control.Monad.ST

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Generic.Mutable as GM

slice :: Int -> Int -> [a] -> [a]
slice begin len = take len . drop begin

l :: [Int]
l = [1..1000]

v :: V.Vector Int
v = V.fromList l

-- n.b. MV.IOVector Int == MV.MVector RealWorld Int
makeMVecIO :: Int -> IO (MV.IOVector Int)
makeMVecIO n = do
  mvec <- GM.new (n + 1)
  go n mvec where
    go 0 v = return v
    go n v = do
      MV.write v n (n * 2)
      go (n - 1) v

makeMVecST :: Int -> V.Vector Int
makeMVecST n = runST $ do
  mvec <- GM.new (n + 1)
  go n mvec where
    go 0 v = V.freeze v
    go n v = do
      MV.write v n (n * 2)
      go (n - 1) v

main :: IO ()
main = defaultMain
  [ bench "slice list" $ whnf (head . slice 100 900) l
  , bench "slice vec" $ whnf (V.head . V.slice 100 900) v
  , bench "make IOVector" $ whnfIO (makeMVecIO 9998)
  , bench "make & freeze STVector" $ whnf makeMVecST 9998
  ]
