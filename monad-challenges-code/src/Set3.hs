module Set3 where

import MCPrelude

import Prelude ()

allPairs [] _ = []
allPairs (x:xs) bs = zip (repeat  x) bs ++ allPairs xs bs

data Card = Card Int String

instance Show Card where
    show (Card r s) = show r ++ s

allCards :: [Int] -> [String] -> [Card]
allCards rs ss = map (uncurry Card) $ allPairs rs ss
