module Queue where

data Queue a = Queue { enqueue :: [a]
                     , dequeue :: [a]
                     } deriving (Eq, Show)

empty :: Queue a
{-# INLINE empty #-}
empty = Queue [] []

pushFront :: a -> Queue a -> Queue a
{-# INLINE pushFront #-}
pushFront x (Queue e d) = Queue (x:e) d

pushBack :: a -> Queue a -> Queue a
{-# INLINE pushBack #-}
pushBack x (Queue e d) = Queue e (x:d)

popFront :: Queue a -> Maybe (a, Queue a)
{-# INLINE popFront #-}
popFront (Queue [] []) = Nothing
popFront (Queue (e:es) d) = Just (e, Queue es d)
popFront (Queue [] d) = popFront $ Queue (reverse d) []

popBack :: Queue a -> Maybe (a, Queue a)
{-# INLINE popBack #-}
popBack (Queue [] []) = Nothing
popBack (Queue e (d:ds)) = Just (d, Queue e ds)
popBack (Queue e []) = popBack $ Queue [] (reverse e)
