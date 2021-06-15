-- This file is insanity. I did not write it.
-- See factorial for an example of what it can do.

{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module Imperative (
  def, var, lit, while, (+=), (-=), (*=)
) where

import Control.Monad (when)
import Control.Monad.ST ( ST, runST )
import Data.STRef ( modifySTRef, newSTRef, readSTRef, STRef )

data R
data L

data Value l s a where
  Var :: STRef s a -> Value l s a
  Lit :: a         -> Value R s a

def :: (forall s. ST s (Value l s a)) -> a
def s = runST (s >>= get)

var   = fmap Var . newSTRef

lit   = Lit

while r f act = do
  rr <- get r
  when (f rr) $ 
    act >> while r f act
  
get (Var r) = readSTRef r
get (Lit v) = return v

modify f (Var r) s = get s >>= modifySTRef r . flip f
  
a += b = modify (+) a b
a -= b = modify (-) a b
a *= b = modify (*) a b

factorial :: Integer -> Integer
factorial n = def $ do
  result <- var 1
  i      <- var n
  while i (>0) $ do
    result *= i
    i      -= lit 1
  return result
