{-# LANGUAGE TupleSections #-}

module Set4 where

import MCPrelude ( Seed )

import Prelude ( fst, ($), foldr, concatMap, (.), flip, id )

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

instance Monad [] where
    return = (:[])
    bind = flip concatMap 

-- I added this for ease, equivalent to transMonad
fmap :: Monad m => (a -> b) -> m a -> m b
fmap f ma = bind ma (return . f)

-- Set1 (generalB), Set2 (yLink), Set3 (allCombs)
liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
-- using only bind and return:
-- liftM2 combin ma mb = bind (bind ma (return . combin)) (bind mb . (.) return)
liftM2 f = ap . fmap f

-- Set1 (repRandom)
sequence :: Monad m => [m a] -> m [a]
sequence = foldr (liftM2 (:)) (return [])

-- Set2 (chain)
(=<<) :: Monad m => (a -> m b) -> m a -> m b
(=<<) = flip bind
infixr 8 =<<

-- Set2 (combine)
join :: Monad m => m (m a) -> m a
join = flip bind id

-- Set3 (combStep)
ap :: Monad m => m (a -> b) -> m a -> m b
ap mf ma = bind mf (`fmap` ma)

-- Set3 (allCombs3)
liftM3 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
liftM3 combin ma = ap . liftM2 combin ma