module Set2 where

import MCPrelude

import Prelude ()

data Maybe a = Just a | Nothing 

instance Show a => Show (Maybe a) where
    show (Just x) = "Just " ++ show x
    show Nothing  = "Nothing"

instance Eq a => Eq (Maybe a) where
    (==) (Just a) (Just b) = a == b
    (==) Nothing  Nothing  = True
    (==) _        _        = False

headMay :: [a] -> Maybe a
headMay []    = Nothing
headMay (x:_) = Just x

tailMay :: [a] -> Maybe [a]
tailMay []     = Nothing
tailMay (a:as) = Just as

lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
lookupMay _ [] = Nothing
lookupMay k ((k0, v0):xs) = if k == k0 then Just v0 else lookupMay k xs

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay _ 0 = Nothing
divMay a b = Just $ a / b

maximumMay :: Ord a => [a] -> Maybe a
maximumMay [] = Nothing
maximumMay (x:xs) = 
    case maximumMay xs of
        Just n -> Just $ if n > x then n else x
        Nothing -> Just x

minimumMay :: Ord a => [a] -> Maybe a
minimumMay []     = Nothing
minimumMay (x:xs) = 
    case maximumMay xs of
        Just n -> Just $ if n > x then n else x
        Nothing -> Just x
