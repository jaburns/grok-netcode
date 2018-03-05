module Simulation(
    Simulation
  , newDefaultSim
  , simGames
  , handleSimEvent
  , updateSim
) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random

import Game (Game, defaultGame, stepGame)
import Input (GameInputs, defaultGameInputs, updateInputsFromEvent)

data Simulation = SimulationState
  { simGames  :: [Game]
  , simRandom :: StdGen
  , simInputs :: GameInputs
  }

newDefaultSim :: IO Simulation
newDefaultSim = do
    stdGen <- newStdGen
    return $ SimulationState [] stdGen defaultGameInputs

handleSimEvent :: Event -> Simulation -> Simulation
handleSimEvent (EventKey (SpecialKey KeyEnter) Down _ _) sim = sim { simGames = defaultGame : (simGames sim) }
handleSimEvent event sim = sim { simInputs = updateInputsFromEvent event (simInputs sim) }

updateSim :: Float -> Simulation -> Simulation
updateSim dt sim = sim { simGames = map (stepGame (simInputs sim)) (simGames sim) }