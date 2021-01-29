module Set2 where

import MCPrelude

import Prelude ()


-------- Maybe Type ---------------------------------

data Maybe a = Just a | Nothing 

instance Show a => Show (Maybe a) where
    show (Just x) = "Just " ++ show x
    show Nothing  = "Nothing"

instance Eq a => Eq (Maybe a) where
    (==) (Just a) (Just b) = a == b
    (==) Nothing  Nothing  = True
    (==) _        _        = False


-------- Prelude Functions ---------------------------

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

-- dont think I"m supposed to have this yet
-- maybe :: (a -> b) -> Maybe a -> Maybe b
-- maybe _ Nothing = Nothing
-- maybe f (Just x) = Just (f x)

---- Example Uses ------------------------------------------

-- forced me to do this expansion yukkk
-- queryGreek :: GreekData -> String -> Maybe Double
-- queryGreek d k = case lookupMay k d of
--     Just xs -> case tailMay xs of
--         Just xtail -> case maximumMay xtail of
--             Just xmax -> case headMay xs of
--                 Just xhead -> divMay (fromIntegral xmax) (fromIntegral xhead)
--                 _ -> Nothing
--             _ -> Nothing
--         _ -> Nothing
--     _ -> Nothing

chain :: (a -> Maybe b) -> Maybe a -> Maybe b
chain _ Nothing = Nothing
chain f (Just x) = f x

link :: Maybe a -> (a -> Maybe b) -> Maybe b
link Nothing  _ = Nothing
link (Just x) f = f x

-- rewrite using chain and link, better but still gross
queryGreek :: GreekData -> String -> Maybe Double
queryGreek d k = let xs = lookupMay k d in 
    case chain maximumMay (chain tailMay xs) of
        Just xmax -> chain (divMay (fromIntegral xmax)) (chain (Just . fromIntegral) $ chain headMay xs)
        _ -> Nothing

yLink :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
yLink f (Just x) (Just y) = Just (f x y)
yLink _ _        _        = Nothing

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries map n0 n1 = yLink (+) (lookupMay n0 map) (lookupMay n1 map)

-- I was told to do this so I did
mkMaybe = Just

transMaybe f = chain (Just . f)

tailProd = transMaybe product . tailMay

tailSum = transMaybe sum . tailMay
