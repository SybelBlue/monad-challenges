module Set3 where

import MCPrelude

import Prelude ()


allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs _ [] _ = []
allCombs f (a:as) bs = map (f a) bs ++ allCombs f as bs

allPairs = allCombs (,)

data Card = Card Int String

instance Show Card where
    show (Card r s) = show r ++ s

allCards = allCombs Card
