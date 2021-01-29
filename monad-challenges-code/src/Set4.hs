module Set4 where

import MCPrelude

import Prelude ()

import Set1
import Set2

-- from Set1
-- generalA :: Gen a -> (a -> b) -> Gen b
-- genTwo :: Gen a -> (a -> Gen b) -> Gen b                 <<< similarity 1 
-- generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c     <<< similarity 2

-- from Set2
-- chain :: (a -> Maybe b) -> Maybe a -> Maybe b
-- link :: Maybe a -> (a -> Maybe b) -> Maybe b             <<< similarity 1
-- yLink :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c  <<< similarity 2

-- Similarity abstractions
-- sim1 :: m a -> (a -> m b) m b
-- sim2 :: (a -> b -> c) -> m a -> m b -> m c

class Monad m where
    bind :: m a -> (a -> m b) -> m b
    return :: a -> m a

instance Monad Maybe where
    return = Just
    bind = link

instance Monad [] where
    return = (:[])
    bind = flip concatMap 
