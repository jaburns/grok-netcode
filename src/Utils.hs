module Utils (
    randomList
  , randomPair
  , randomUnitAndList
  , lineSegmentsIntersect
  , clamp
) where


import Control.Monad
import Control.Monad.Trans.State
import System.Random


randomList :: (RandomGen g, Random a) => Int -> g -> ([a], g)
randomList count = runState $ replicateM count (state random)


randomPair :: (RandomGen g, Random a, Random b) => g -> ((a, b), g)
randomPair rng = 
    ((a, b), rng'')
  where
    (a, rng' ) = random rng
    (b, rng'') = random rng'


randomUnitAndList :: (RandomGen g, Random a, Random b) => Int -> g -> ((a, [b]), g)
randomUnitAndList count rng = 
    ((a, b), rng'')
  where
    (a, rng' ) = random rng
    (b, rng'') = randomList count rng'


clamp :: Ord a => a -> a -> a -> a
clamp mini maxi x = if x < mini then mini else if x > maxi then maxi else x


lineSegmentsIntersect :: (Num a, Ord a) => (a, a) -> (a, a) -> (a, a) -> (a, a) -> Bool
lineSegmentsIntersect (x00, y00) (x01, y01) (x10, y10) (x11, y11) = 
    numa >= 0 && numa <= denom && numb >= 0 && numb <= denom
  where 
    denom = dy4y3*dx2x1 - dx4x3*dy2y1
    numa  = dx4x3*dy1y3 - dy4y3*dx1x3
    numb  = dx2x1*dy1y3 - dy2y1*dx1x3
    dx1x3 = x00 - x10
    dy1y3 = y00 - y10
    dx2x1 = x01 - x00
    dy2y1 = y01 - y00
    dx4x3 = x11 - x10
    dy4y3 = y11 - y10