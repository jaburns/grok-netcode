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

import Network (Network, newNetworkFromSeed, updateNetwork)
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

randoms' :: RandomGen g => Int -> g -> ([Float], g)
randoms' n = runState (replicateM n (state random))

newRandomSimulation :: IO Simulation
newRandomSimulation = do
    firstRNG <- newStdGen
    let (networkSeed, newRNG) = random firstRNG
    return $ Simulation' newRNG defaultGameInputs clients server (newNetworkFromSeed networkSeed)
  where
    server = (Server' (newGame (0,0)))
    clients = [(Client' (newGame (0,0))), (Client' (newGame (0,0)))]

newSimulationFromSeed :: Float -> Simulation
newSimulationFromSeed seed =
    Simulation' (read . show $ seed) defaultGameInputs [] (Server' (newGame (0,0))) (newNetworkFromSeed (seed + 1))

handleSimEvent :: Event -> Simulation -> Simulation
handleSimEvent event sim = sim { simInputs = updateInputsWithEvent wasdMapping event (simInputs sim) }

updateSim :: Float -> Simulation -> Simulation
updateSim dt = execState $ do
    modify $ \sim -> sim { simNetwork = updateNetwork dt (simNetwork sim) }
    modify $ \sim -> sim { simServer = updateServer (simInputs sim) (simServer sim) }
    modify updateClients
  where
    updateClients sim = let game = serverGame . simServer $ sim
      in sim { simClients = map (updateClient game) (simClients sim) }

updateServer :: GameInputs -> Server -> Server
updateServer inputs (Server' game) = Server' (stepGame inputs game)

updateClient :: Game -> Client -> Client
updateClient game _ = Client' game

getRenderableGames :: Simulation -> [Game]
getRenderableGames (Simulation' _ _ clients (Server' serverGame) _) = [serverGame] ++ clientGames
  where clientGames = map clientGame clients