module LogPractice where

import Data.Functor ( ($>) )

-- Managing side effects
data Log a = Log a String

instance Functor Log where
    f `fmap` (Log a log) = Log (f a) log

instance Applicative Log where
    pure = return
    (Log f s) <*> (Log arg t) = Log (f arg) (s ++ t)

instance Monad Log where
    return = flip Log ""
    (Log arg s) >>= f =
        let (Log res t) = f arg in Log res (s ++ t)

printLog :: Log a -> IO a
printLog (Log x log) = putStrLn log $> x

lNot :: Bool -> Log Bool 
lNot b = Log (not b) "not!"

lPos :: (Ord a, Num a) => a -> Log Bool
lPos x = Log (x > 0) "pos?"

lAdd :: Num a => a -> a -> Log a
lAdd x y = Log (x + y) "add!"

main :: IO Bool
main = printLog $ lNot =<< lPos =<< lAdd 3 4
