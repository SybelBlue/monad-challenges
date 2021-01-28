module Set1 where

import MCPrelude
import Data.Tuple (fst, snd)

main = putStrLn $ if check then "Ok!" else "Err " ++ show fiveRands

nRands n = map fst . take n $ iterate (rand . snd) (rand $ mkSeed 1)

fiveRands = nRands 5

check = checkProd == product fiveRands
    where checkProd = 8681089573064486461641871805074254223660
