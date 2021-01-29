{-# LANGUAGE TupleSections #-}

module Set4 where

import MCPrelude

import Prelude ()

import Data.Tuple (fst)

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

-- type synonyms can't have instances, but newtypes,
-- which are as fast and light as type synonyms, can.
-- so we remake the Gen type with the same basic signature
newtype Gen a = Gen { gen :: Seed -> (a, Seed) }

evalGen :: Gen a -> Seed -> a
evalGen g = fst . gen g

instance Monad Gen where
    return x = Gen (x,)
    bind ga fgb = Gen $ \s -> let (r, n) = gen ga s in gen (fgb r) n