module Utils (
    randomList
  , randomPair
  , randomUnitAndList
) where


import Control.Monad
import Control.Monad.Trans.State
import Data.Maybe
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


lineSegmentIntersects 
    :: (Float, Float) -> (Float, Float) 
    -> (Float, Float) -> (Float, Float) 
    -> Bool
lineSegmentIntersects a b c d = isJust $ lineSegmentIntersection a b c d


lineSegmentIntersection 
    :: (Float, Float) -> (Float, Float) 
    -> (Float, Float) -> (Float, Float) 
    -> Maybe (Float, Float)
lineSegmentIntersection (x00, y00) (x01, y01) (x10, y10) (x11, y11) = undefined