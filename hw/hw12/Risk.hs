{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List (sort, foldl')

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

data Battlefield = Battlefield { attackers :: Army, defenders :: Army } deriving Show

battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield a d) = do
  let a' = min 3 (a - 1)
      d' = min 2 d
  as <- reverse . sort <$> replicateM a' die
  bs <- reverse . sort <$> replicateM d' die
  let (aWins, dWins) = foldl' f (0, 0) $ zip as bs
      f (aw, dw) (a, b) | a > b     = (aw + 1, dw)
                        | otherwise = (aw, dw + 1)
  return $ Battlefield (a - dWins) (d - aWins)

-- | battle
-- >>> evalRandIO $ battle (Battlefield 3 2)

invade :: Battlefield -> Rand StdGen Battlefield
invade b@(Battlefield a d)
  | a < 2 || d == 0 = return b
  | otherwise = battle b >>= invade

successProb :: Battlefield -> Rand StdGen Double
successProb b = do
  let n = 1000
  bs <- replicateM n $ invade b
  let wins = length $ filter ((== 0) . defenders) bs
  return $ fromIntegral wins / fromIntegral n

-- | successProb
-- >>> evalRandIO $ successProb (Battlefield 3 2)
-- 0.363

-- >>> evalRandIO $ successProb (Battlefield 3 1)
-- 0.736

-- >>> evalRandIO $ successProb (Battlefield 4 3)
-- 0.454

-- >>> evalRandIO $ successProb (Battlefield 5 3)
-- 0.641

-- >>> evalRandIO $ successProb (Battlefield 6 3)
-- 0.759
