module Main where

import Control.Monad.Trans.State

get' :: State s s
get' = state $ \s -> (s, s)

put' :: s -> State s ()
put' s' = state $ \_ -> ((), s')

exec' :: State s a -> s -> s
exec' = (snd .) . runState

eval' :: State s a -> s -> a
eval' = (fst .) . runState

modify' :: (s -> s) -> State s ()
modify' = (get' >>=) . (put' .)

main :: IO ()
main = putStrLn "whatever"
