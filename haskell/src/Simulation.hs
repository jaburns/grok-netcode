module Simulation(
    Simulation
  , newDefaultSim
  , simGames
  , handleSimEvent
  , updateSim
) where

import Control.Monad
import Control.Monad.Trans.State
import Graphics.Gloss.Interface.Pure.Game
import System.Random

import Game (Game, newGame, stepGame)
import Input (GameInputs, defaultGameInputs, wasdMapping, updateInputsWithEvent)

data Simulation = Simulation'
  { simGames  :: [Game]
  , simRandom :: StdGen
  , simInputs :: GameInputs
  }

randoms' :: Int -> StdGen -> ([Float], StdGen)
randoms' n = runState (replicateM n (state random))

newDefaultSim :: IO Simulation
newDefaultSim = do
    stdGen <- newStdGen
    return $ Simulation' [] stdGen defaultGameInputs

addGame :: Simulation -> Simulation
addGame sim =  sim
  { simGames = builtGame : (simGames sim)
  , simRandom = newRandom
  }
  where
    (x:y:[], newRandom) = randoms' 2 (simRandom sim)
    builtGame = newGame (-100 + 200 * x, -100 + 200 * y)

handleSimEvent :: Event -> Simulation -> Simulation
handleSimEvent (EventKey (SpecialKey KeyEnter) Down _ _) sim = addGame sim
handleSimEvent event sim = sim { simInputs = updateInputsWithEvent wasdMapping event (simInputs sim) }

updateSim :: Float -> Simulation -> Simulation
updateSim _ sim = sim { simGames = map (stepGame (simInputs sim)) (simGames sim) }