-- {-# LANGUAGE TupleSections #-}

module Set1 where

import MCPrelude
import Data.Char (chr, ord)
import Data.Tuple (fst)

type Gen a = Seed -> (a, Seed)

main = do
    putStrLn $ if fiveRandsCheck then "Ok!" else "Err " ++ show fiveRands
    putStrLn $ if randString3Check then "Ok!" else "Err " ++ show randString3
    putStrLn $ if randEvenOddTenCheck then "Ok!" else "Err " ++ show randEvenOddTenCheck

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
randOdd = generalA randEven succ

randTen :: Gen Integer 
randTen = generalA rand (*10)

generalA f g s = let (v, n) = f s in (g v, n)

fiveRandsCheck = checkProd == product fiveRands
    where checkProd = 8681089573064486461641871805074254223660

randString3Check = randString3 == "lrf"

randEvenOddTenCheck = checkProd == product (map (\f -> fst $ f $ mkSeed 1) [randEven, randOdd, randTen])
    where checkProd = 189908109902700
