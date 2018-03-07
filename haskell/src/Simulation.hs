module Simulation(
    Simulation
  , newRandomSimulation
  , simClients, clientGame
  , handleSimEvent
  , updateSim
  , getRenderableGames
) where

import Control.Monad
import Control.Monad.Trans.State
import Graphics.Gloss.Interface.Pure.Game
import System.Random

import Network (Network, newNetworkFromSeed)
import Game (Game, newGame, stepGame)
import Input (GameInputs, TaggedInputs, defaultGameInputs, wasdMapping, updateInputsWithEvent)

data Simulation = Simulation'
  { simRandom  :: StdGen
  , simInputs  :: GameInputs
  , simClients :: [Client]
  , simServer  :: Server
  , simNetwork :: Network TaggedInputs Game
  }

data Client = Client'
  { clientGame :: Game
  }

data Server = Server'
  { serverGame :: Game
  }

randoms' :: Int -> StdGen -> ([Float], StdGen)
randoms' n = runState (replicateM n (state random))

newRandomSimulation :: IO Simulation
newRandomSimulation = do
    firstRNG <- newStdGen
    let (networkSeed, newRNG) = random firstRNG
    return $ Simulation' newRNG defaultGameInputs [] (Server' (newGame (0,0))) (newNetworkFromSeed networkSeed)

newSimulationFromSeed :: Float -> Simulation
newSimulationFromSeed seed =
    Simulation' (read . show $ seed) defaultGameInputs [] (Server' (newGame (0,0))) (newNetworkFromSeed (seed + 1))

addGame :: Simulation -> Simulation
addGame sim = undefined --sim
{--
  { simGames = builtGame : (simGames sim)
  , simRandom = newRandom
  }
  where
    (x:y:[], newRandom) = randoms' 2 (simRandom sim)
    builtGame = newGame (-100 + 200 * x, -100 + 200 * y)
    --}

handleSimEvent :: Event -> Simulation -> Simulation
handleSimEvent (EventKey (SpecialKey KeyEnter) Down _ _) sim = addGame sim
handleSimEvent event sim = sim { simInputs = updateInputsWithEvent wasdMapping event (simInputs sim) }

updateSim :: Float -> Simulation -> Simulation
updateSim _ sim = sim { simServer = updateServer (simInputs sim) (simServer sim) }

updateServer :: GameInputs -> Server -> Server
updateServer inputs (Server' game) = Server' (stepGame inputs game)

getRenderableGames :: Simulation -> [Game]
getRenderableGames (Simulation' _ _ _ (Server' game) _) = [game]