{-# LANGUAGE TupleSections #-}

module Set1 where

import Prelude 
    ( Char, Integer, Int, IO, Bool
    , ($), (.), (+), (*), (==), (++)
    , uncurry, fromIntegral, mod, flip, take, putStrLn, product, show, replicate, map
    )

import MCPrelude ( Seed, mkSeed )
import qualified MCPrelude as MCP

import Data.Char (chr, ord)
import Data.Tuple (fst)

import Set4

-- type synonyms can't have instances, but newtypes,
-- which are as fast and light as type synonyms, can.
-- so we remake the Gen type with the same basic signature
newtype Gen a = Gen { runGen :: Seed -> (a, Seed) }

evalGen :: Gen a -> Seed -> a
evalGen g = fst . runGen g

instance Monad Gen where
    return = Gen . (,)
    bind ga fgb = Gen $ \s -> let (r, n) = runGen ga s in runGen (fgb r) n


iterateRand1 :: Gen a -> [a]
iterateRand1 = flip iterateRand (mkSeed 1)

iterateRand :: Gen a -> Seed -> [a]
iterateRand r s = let (v, n) = runGen r s in v : iterateRand r n

iterateNRand1 :: Int -> Gen a -> [a]
iterateNRand1 n = take n . iterateRand1

rand :: Gen Integer
rand = Gen MCP.rand

fiveRands :: [Integer]
fiveRands = iterateNRand1 5 rand

randLetter :: Gen Char
randLetter = (chr . (+ ord 'a') . fromIntegral . (`mod` 26)) `fmap` rand

randString3 :: [Char]
randString3 = iterateNRand1 3 randLetter

randEven :: Gen Integer 
randEven = (*2) `fmap` rand

randOdd :: Gen Integer 
randOdd = (+1) `fmap` randEven

randTen :: Gen Integer 
randTen = (*10) `fmap` rand

-- became flip fmap
-- generalA :: Gen a -> (a -> b) -> Gen b
-- -- generalA :: (p -> (t, b)) -> (t -> a) -> p -> (a, b)
-- generalA ga f = Gen $ \s -> let (v, n) = runGen ga s in (f v, n)

randPair :: Gen (Char, Integer)
randPair = liftM2 (,) randLetter rand
-- equivalently: 
-- Gen $ \s -> do
--     let (l, n) = runGen randLetter s
--     let (d, final) = runGen rand n
--     ((l, d), final)

-- made unneccessary by liftM2
-- generalPair :: (p -> (a, b1)) -> (b1 -> (b2, b3)) -> p -> ((a, b2), b3)
-- generalPair :: Gen a -> Gen b -> Gen (a, b)
-- generalPair = generalB (,)
--   do
--     let (r1, s1) = f s
--     let (r2, s2) = g s1
--     ((r1, r2), s2)

-- liftM2 after made into monad
-- generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
-- generalB :: (t1 -> t2 -> a) -> (p -> (t1, b1)) -> (b1 -> (t2, b2)) -> p -> (a, b2)
-- generalB combin f g = Gen $ \s -> do
--     let (r1, s1) = runGen f s
--     let (r2, s2) = runGen g s1
--     (combin r1 r2, s2)

-- sequence after made into monad
-- repRandom :: [Gen a] -> Gen [a]
-- -- repRandom :: [t -> (a, t)] -> t -> [a]
-- repRandom [] = ([],)
-- repRandom (g:gs) = foldr (generalB (:)) ([],) gs

-- >>= (aka bind after made into monad)
-- genTwo :: Gen a -> (a -> Gen b) -> Gen b
-- genTwo gen fn = uncurry fn . gen

-- return after made into monad
-- mkGen :: a -> Gen a
-- mkGen = (,)

--             TEST ITEMS FROM HERE ON          ---------------

main :: IO ()
main = do
    putStrLn $ if fiveRandsCheck then "Ok!" else "Err " ++ show fiveRands
    putStrLn $ if randString3Check then "Ok!" else "Err " ++ show randString3
    putStrLn $ if randEvenOddTenCheck then "Ok!" else "Err " ++ show randEvenOddTenCheck
    putStrLn $ if repRandomCheck then "Ok!" else "Err " ++ show (runGen (sequence (replicate 3 randLetter)) (mkSeed 1)) ++ show randString3

fiveRandsCheck :: Bool
fiveRandsCheck = checkProd == product fiveRands
    where checkProd = 8681089573064486461641871805074254223660

randString3Check :: Bool
randString3Check = randString3 == "lrf"

randEvenOddTenCheck :: Bool
randEvenOddTenCheck = checkProd == product (map (\f -> fst $ runGen f $ mkSeed 1) [randEven, randOdd, randTen])
    where checkProd = 189908109902700

repRandomCheck :: Bool
repRandomCheck = fst (runGen (sequence (replicate 3 randLetter)) (mkSeed 1)) == randString3
