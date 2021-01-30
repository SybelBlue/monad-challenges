{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}

module BoardPractice where

import Data.Tuple ( fst, snd )

import Control.Applicative ( Applicative(liftA2) )
import Control.Monad

mapFst f (a, c) = (f a, c)

toMaybe b x = if b then Just x else Nothing

data Board = Board
    { temp :: Int
    , o2 :: Int
    , oceans :: Int
    , tr :: Int
    } deriving (Show)

newtype GameAct a = GameAct { runAction :: Board -> (a, Board) }

mutatingAct f = GameAct $ \b -> ((), f b)

failableAct f = GameAct $ \b -> maybe (False, b) (True,) (f b)

simpleAct f = GameAct $ \b -> (f b, b)

instance Functor GameAct where
    f `fmap` ga = GameAct $ mapFst f . runAction ga

instance Applicative GameAct where
    pure = return
    -- Applicative may be implemented with liftA2 *or* <*>
    -- liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
    liftA2 fab ga gb = GameAct $ \board ->
        let (a, b0) = runAction ga board
            (b, b1) = runAction gb b0 in
            (fab a b, b1)

instance Monad GameAct where
    return = GameAct . (,)
    ga >>= fb = GameAct $ \board -> 
        let (res, next) = runAction ga board in runAction (fb res) next

startingBoard = Board { temp = -30, o2 = 0, oceans = 0, tr = 0 }

increaseO2 = failableAct $ \b -> toMaybe (o2 b < 14) (b { o2 = o2 b + 1 })

increaseTemp = failableAct $ \b -> toMaybe (temp b < 8) (b { temp = temp b + 1 })

increaseOceans = failableAct $ \b -> toMaybe (oceans b < 9) (b { oceans = oceans b + 1 })

drop :: GameAct a -> (a -> Board -> b) -> Board -> b
drop ga f = uncurry f . runAction ga

gainTRFor :: [GameAct Bool] -> GameAct Int
gainTRFor = sequence >=> gainTR . length . filter id 

gainTR n = const n `fmap` mutatingAct (\b -> b { tr = n + tr b })

runFromStart = flip runAction startingBoard
