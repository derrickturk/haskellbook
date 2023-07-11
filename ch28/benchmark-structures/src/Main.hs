module Main where

import Criterion.Main
import Data.List (unfoldr)
import qualified Data.Map as M
import qualified Data.Sequence as S

assocList :: Int -> [(String, Int)]
assocList = unfoldr f where
  f n | n < 0 = Nothing
  f n         = Just ((show n, n), n - 1)

pairList :: [(String, Int)]
pairList = assocList 90001

testMap :: M.Map String Int
testMap = M.fromList pairList

pairSeq :: S.Seq (String, Int)
pairSeq = S.fromList pairList

main :: IO ()
main = defaultMain
  [ bench "alist, not found" $ whnf (lookup "nope") pairList
  , bench "alist, good key" $ whnf (lookup "87032") pairList
  , bench "alist, bad key" $ whnf (lookup "3") pairList
  , bench "map, not found" $ whnf (M.lookup "nope") testMap
  , bench "map, good key" $ whnf (M.lookup "87032") testMap
  , bench "map, bad key" $ whnf (M.lookup "3") testMap
  , bench "indexing list" $ whnf (!! 80000) pairList
  , bench "indexing seq" $ whnf (flip S.index 80000) pairSeq
  ]
