module SideEffectPractice where

import Data.Bifunctor (Bifunctor(first))

newtype SE s a = SE { runSE :: s -> (a, s) }

instance Functor (SE s) where
    fmap fab sa = SE $ first fab . runSE sa

instance Applicative (SE s) where
    pure = return
    sfab <*> sa = SE $ \s -> let (fab, n) = runSE sfab s in first fab (runSE sa n)

instance Monad (SE s) where
    return = SE . (,)
    a >>= fb = SE $ uncurry (runSE . fb) . runSE a


