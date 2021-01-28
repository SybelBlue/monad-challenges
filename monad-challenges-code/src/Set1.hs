{-# LANGUAGE TupleSections #-}

module Set1 where

import MCPrelude
import Data.Char (chr, ord)

main = do
    putStrLn $ if fiveRandsCheck then "Ok!" else "Err " ++ show fiveRands
    putStrLn $ if randString3Check then "Ok!" else "Err " ++ show randString3

iterateRand1 = flip iterateRand (mkSeed 1)

iterateRand :: (Seed -> (a, Seed)) -> Seed -> [a]
iterateRand r s = let (v, n) = r s in v : iterateRand r n

iterateNRand1 n = take n . iterateRand1

fiveRands = iterateNRand1 5 rand

randLetter = uncurry ((,) . chr . (+ ord 'a') . fromIntegral . (`mod` 26)) . rand

randString3 = iterateNRand1 3 randLetter

fiveRandsCheck = checkProd == product fiveRands
    where checkProd = 8681089573064486461641871805074254223660

randString3Check = randString3 == "lrf"