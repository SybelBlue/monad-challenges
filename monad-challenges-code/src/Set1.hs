{-# LANGUAGE TupleSections #-}

module Set1 where

import Prelude 
    ( Char, Integer
    , ($), (.), (+), (*), (==), (++)
    , uncurry, fromIntegral, mod, flip, take, putStrLn, product, show, replicate, map
    )

import MCPrelude ( Seed, mkSeed, rand )
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
    return x = Gen (x,)
    bind ga fgb = Gen $ \s -> let (r, n) = runGen ga s in runGen (fgb r) n

iterateRand1 = flip iterateRand (mkSeed 1)

iterateRand :: Gen a -> Seed -> [a]
iterateRand r s = let (v, n) = r s in v : iterateRand r n

iterateNRand1 n = take n . iterateRand1

fiveRands = iterateNRand1 5 rand

randLetter :: Gen Char
randLetter = uncurry ((,) . chr . (+ ord 'a') . fromIntegral . (`mod` 26)) . rand

randString3 = iterateNRand1 3 randLetter

randEven :: Gen Integer 
randEven = generalA rand (*2)

randOdd :: Gen Integer 
randOdd = generalA randEven (+1)

randTen :: Gen Integer 
randTen = generalA rand (*10)

generalA :: Gen a -> (a -> b) -> Gen b
-- generalA :: (p -> (t, b)) -> (t -> a) -> p -> (a, b)
generalA f g s = let (v, n) = f s in (g v, n)

randPair :: Gen (Char, Integer)
-- randPair :: Seed -> ((Char, Integer), Seed)
randPair s = do
    let (l, n) = randLetter s
    let (d, final) = rand n
    ((l, d), final)

randPair_ = generalPair randLetter rand

-- generalPair :: (p -> (a, b1)) -> (b1 -> (b2, b3)) -> p -> ((a, b2), b3)
generalPair :: Gen a -> Gen b -> Gen (a, b)
generalPair = generalB (,)
--   do
--     let (r1, s1) = f s
--     let (r2, s2) = g s1
--     ((r1, r2), s2)

generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
-- generalB :: (t1 -> t2 -> a) -> (p -> (t1, b1)) -> (b1 -> (t2, b2)) -> p -> (a, b2)
generalB combin f g s = do
    let (r1, s1) = f s
    let (r2, s2) = g s1
    (combin r1 r2, s2)

generalB2 :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB2 f ga gb = uncurry (generalA gb) . generalA ga f
-- generalB2 f ga gb = uncurry (\f -> genTwo gb (mkGen . f)) . genTwo ga (mkGen . f)

repRandom :: [Gen a] -> Gen [a]
-- repRandom :: [t -> (a, t)] -> t -> [a]
repRandom [] = ([],)
repRandom (g:gs) = generalB (:) g (repRandom gs)

-- >>= (aka bind)
genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo gen fn = uncurry fn . gen

-- return
mkGen :: a -> Gen a
mkGen = (,)

--             TEST ITEMS FROM HERE ON          ---------------

main = do
    putStrLn $ if fiveRandsCheck then "Ok!" else "Err " ++ show fiveRands
    putStrLn $ if randString3Check then "Ok!" else "Err " ++ show randString3
    putStrLn $ if randEvenOddTenCheck then "Ok!" else "Err " ++ show randEvenOddTenCheck
    putStrLn $ if generalPairCheck then "Ok!" else "Err " ++ show (randPair (mkSeed 1)) ++ show (randPair_ (mkSeed 1))
    putStrLn $ if repRandomCheck then "Ok!" else "Err " ++ show (repRandom (replicate  3 randLetter) (mkSeed 1)) ++ show randString3

fiveRandsCheck = checkProd == product fiveRands
    where checkProd = 8681089573064486461641871805074254223660

randString3Check = randString3 == "lrf"

randEvenOddTenCheck = checkProd == product (map (\f -> fst $ f $ mkSeed 1) [randEven, randOdd, randTen])
    where checkProd = 189908109902700

generalPairCheck = randPair (mkSeed 1) == randPair_ (mkSeed 1)

repRandomCheck = fst (repRandom (replicate 3 randLetter) (mkSeed 1)) == randString3
