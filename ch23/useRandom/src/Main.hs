module Main where

import System.Random
import Control.Monad.Trans.State
import Control.Applicative (liftA3)
import Control.Monad (replicateM)

data Die = DieOne
         | DieTwo
         | DieThree
         | DieFour
         | DieFive
         | DieSix
         deriving (Eq, Show)

intToDie :: Int -> Die
intToDie 1 = DieOne
intToDie 2 = DieTwo
intToDie 3 = DieThree
intToDie 4 = DieFour
intToDie 5 = DieFive
intToDie 6 = DieSix
intToDie _ = error "not a valid die value"

dieToInt :: Die -> Int
dieToInt DieOne = 1
dieToInt DieTwo = 2
dieToInt DieThree = 3
dieToInt DieFour = 4
dieToInt DieFive = 5
dieToInt DieSix = 6

randomDie :: RandomGen g => g -> (Die, g)
randomDie s = (intToDie x, s') where
  (x, s') = randomR (1, 6) s

rollDie :: RandomGen g => State g Die
rollDie = state randomDie

rollThreeDice :: RandomGen g => State g (Die, Die, Die)
rollThreeDice = liftA3 (,,) rollDie rollDie rollDie

rollDice :: RandomGen g => Int -> State g [Die]
-- rollDice = sequenceA . flip replicate rollDie
-- which is its own thing already!
rollDice = flip replicateM rollDie

rollsTil :: RandomGen g => Int -> State g Int
rollsTil n = do
  d <- dieToInt <$> rollDie
  if d >= n
    then return 1
    else (+ 1) <$> rollsTil (n - d)

loggingRollsTil :: RandomGen g => Int -> State g (Int, [Die])
loggingRollsTil n = do
  d <- rollDie
  let rem = n - dieToInt d
  if rem <= 0
    then return (1, [d])
    else do
      (rest, rolls) <- loggingRollsTil rem
      return (1 + rest, d:rolls)

main :: IO ()
main = do
  let (count, rolls) = loggingRollsTil 200 `evalState` mkStdGen 17777
  putStrLn $ "got to 200 in " ++ show count ++ " rolls:"
  print $ dieToInt <$> rolls
