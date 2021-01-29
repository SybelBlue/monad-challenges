module Set3 where

import MCPrelude

import Prelude ()


allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
-- allCombs _ _ [] = []
-- allCombs _ [] _ = []
-- allCombs f (a:as) bs = map (f a) bs ++ allCombs f as bs
allCombs f as = combStep (map f as)

allPairs = allCombs (,)

data Card = Card Int String

instance Show Card where
    show (Card r s) = show r ++ s

allCards = allCombs Card

allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
-- allCombs3 _ [] _ _ = []
-- allCombs3 _ _ [] _ = []
-- allCombs3 _ _ _ [] = []
-- allCombs3 f (a:as) bs cs = map (uncurry $ f a) (allPairs bs cs) ++ allCombs3 f as bs cs
allCombs3 f as bs = combStep (allCombs f as bs)

combStep :: [a -> b] -> [a] -> [b]
combStep _ [] = []
combStep [] _ = []
combStep (f:fs) as = map f as ++ combStep fs as

allCombs4 :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
allCombs4 f as bs cs = combStep (allCombs3 f as bs cs)
-- can be continued forever, that is the power of combStep
