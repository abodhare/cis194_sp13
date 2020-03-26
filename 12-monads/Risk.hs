{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving Show

dieRoll :: Army -> Rand StdGen [DieValue]
dieRoll x = replicateM x getRandom

dieRolls :: (Army, Army) -> Rand StdGen ([DieValue], [DieValue])
dieRolls (x, y) = dieRoll x >>= (\a -> dieRoll y >>= (\b -> return (a, b)))

maxUnits :: Battlefield -> (Army, Army)
maxUnits (Battlefield x y) = (min 3 (x - 1), min 2 y)

-- Evaluate wins for attackers and defenders
evaluate :: ([DieValue], [DieValue]) -> (Int, Int)
evaluate (a, b) = foldr (calc . win . uncurry (-)) (0, 0) (zip (sort a) (sort b))
  where win x | x > 0     = (1, 0)
              | otherwise = (0, 1)
        calc (x, y) (zx, zy) = (x + zx, y + zy)

-- The second argument is the (wins for attackers, wins for defenders)
update :: Battlefield -> (Int, Int) -> Battlefield
update (Battlefield x y) (a, b) = Battlefield (x - b) (y - a)

battle :: Battlefield -> Rand StdGen Battlefield
battle x = update x . evaluate <$> dieRolls (maxUnits x)

invade :: Battlefield -> Rand StdGen Battlefield
invade x = battle x >>= check
  where check a
          | attackers a < 2 || defenders a == 0 = return a
          | otherwise                           = invade a

successProb :: Battlefield -> Rand StdGen Double
successProb x = (/ 1000) . sum . map win <$> replicateM 1000 (invade x)
  where win a | defenders a == 0 = 1
              | attackers a == 1 = 0
              | otherwise        = 0

test :: Battlefield -> IO ()
test x = evalRandIO (successProb x) >>= print
