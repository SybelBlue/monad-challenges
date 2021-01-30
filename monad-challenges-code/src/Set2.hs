module Set2 where

import MCPrelude

import Set4

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

instance Monad Maybe where
    return = Just
    bind Nothing _ = Nothing
    bind (Just x) f = f x


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

-- replaced by =<< (aka flip bind)
-- chain :: (a -> Maybe b) -> Maybe a -> Maybe b
-- chain _ Nothing = Nothing
-- chain f (Just x) = f x

-- replaced by bind
-- link :: Maybe a -> (a -> Maybe b) -> Maybe b
-- link Nothing  _ = Nothing
-- link (Just x) f = f x

queryGreek :: GreekData -> String -> Maybe Double
queryGreek d k = join $ liftM2 divMay mnum mdenom
    where 
        xs = lookupMay k d
        mnum = fmap fromIntegral $ maximumMay =<< tailMay =<< xs
        mdenom = fmap fromIntegral $ headMay =<< xs

-- replaced by liftM2
-- yLink :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
-- yLink f (Just x) (Just y) = Just (f x y)
-- yLink _ _        _        = Nothing

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries map n0 n1 = liftM2 (+) (lookupMay n0 map) (lookupMay n1 map)

-- replaced by return
-- mkMaybe = Just

-- replaced by fmap
-- transMaybe Nothing _ = Nothing
-- transMaybe (Just x) f = Just (f x)

tailProd :: [Integer] -> Maybe Integer
tailProd = fmap product . tailMay

tailSum :: [Integer] -> Maybe Integer
tailSum = fmap sum . tailMay

-- gross, again this is the signature I was given
tailMax :: Ord a => [a] -> Maybe (Maybe a)
tailMax = fmap maximumMay . tailMay

tailMin :: Ord a => [a] -> Maybe (Maybe a)
tailMin = fmap minimumMay . tailMay

-- replaced by join
-- combine (Just x) = x
-- combine _        = Nothing
