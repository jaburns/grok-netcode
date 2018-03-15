module Simulation(
    Simulation
  , newSimulation
  , handleSimEvent
  , updateSim
  , renderSim
) where


import Control.Monad
import Control.Monad.Trans.State
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Map.Merge.Strict as M
import Graphics.Gloss.Interface.Pure.Game
import System.Random

import Network (Network, newNetwork, updateNetwork, clientReceivePackets, clientSendPackets, 
    serverReceivePackets, serverSendPackets, clearPacketQueues)
import Game (Game, PlayerID, newGame, gameFrame, renderClientGame, renderServerGame, addPlayerToGame, 
    stepServerGame, predictClientGame)
import Input (PlayerInput, AllInputs, KeyMapping(..), newInputs, updateInputsWithEvent, readPlayerInput)
import Palette(oneColor, fgColor, twoColor)


type ServerPacket = Game
type ClientPacket = (PlayerID, PlayerInput)

type SimNetwork = Network ClientPacket ServerPacket

data Simulation = Simulation'
  { simRandom  :: StdGen
  , simInputs  :: AllInputs
  , simClients :: [Client]
  , simServer  :: Server
  , simNetwork :: SimNetwork
  }

data Client = Client'
  { clientPlayerID     :: PlayerID
  , clientKeyMapping   :: KeyMapping
  , clientInputHistory :: [PlayerInput]
  , clientGame         :: Game
  }

data Server = Server'
  { serverGameHistory  :: [Game]
  , serverInputBuffers :: M.Map PlayerID [PlayerInput]
  }


newSimulation :: StdGen -> Simulation 
newSimulation rng = Simulation' rng2 newInputs [client0, client1] server net
  where
    (id0, game0, rng0) = addPlayerToGame rng  newGame
    (id1, game1, rng1) = addPlayerToGame rng0 game0
    client0 = Client' id0 Arrows [] newGame
    client1 = Client' id1 WASD [] newGame
    server = Server' [game1] M.empty
    (rng2, netRNG) = split rng1
    net = newNetwork netRNG

handleSimEvent :: Event -> Simulation -> Simulation
handleSimEvent event sim = sim { simInputs = updateInputsWithEvent event (simInputs sim) }

updateSim :: Float -> Simulation -> Simulation
updateSim dt = execState $ do
    modify $ \sim -> sim { simNetwork = updateNetwork dt (simNetwork sim) }
    modify updateServerInSimulation
    modify updateClientsInSimulation
    modify $ \sim -> sim { simNetwork = clearPacketQueues (simNetwork sim) }


updateClientsInSimulation :: Simulation -> Simulation
updateClientsInSimulation sim = sim
  { simClients = map snd newClientsWithPackets 
  , simRandom = rng'
  , simNetwork = clientSendPackets (concat . map fst $ newClientsWithPackets) (simNetwork sim)
  }
  where
    packets = clientReceivePackets (simNetwork sim)
    (clientInputs, rng') = readInputsForClients (simInputs sim) (simClients sim) (simRandom sim)
    newClientsWithPackets = zipWith (updateClient packets) clientInputs (simClients sim)

readInputsForClients :: AllInputs -> [Client] -> StdGen -> ([PlayerInput], StdGen)
readInputsForClients allInputs clients = runState getInputs
  where
    getInputs = mapM getInput clients
    getInput client = state $ readPlayerInput allInputs (clientKeyMapping client)

updateClient :: [ServerPacket] -> PlayerInput -> Client -> ([ClientPacket], Client)
updateClient serverGames inputs client = ([(clientPlayerID client, inputs)], newClient)
  where
    newClient = client 
      { clientGame = predictClientGame (clientPlayerID client) newInputs' newGame'
      , clientInputHistory = newInputs'
      }
    newInputs' = take 60 $ inputs : clientInputHistory client
    newGame' = case serverGames of 
                 (game:_) -> latestGame game (clientGame client)
                 []       -> clientGame client
    latestGame new old = if gameFrame new >= gameFrame old then new else old


updateServerInSimulation :: Simulation -> Simulation
updateServerInSimulation sim = sim 
  { simServer = newServer
  , simNetwork = serverSendPackets outgoingPackets (simNetwork sim)
  }
  where
    packets = serverReceivePackets (simNetwork sim)
    (outgoingPackets, newServer) = updateServer packets (simServer sim) 

updateServer :: [ClientPacket] -> Server -> ([ServerPacket], Server)
updateServer inputPackets server = ([head . serverGameHistory $ newServer], newServer)
  where 
    updatedInputBuffers = M.merge (M.dropMissing) (M.preserveMissing) (M.zipWithMaybeMatched updateBuffer) 
                                  (mapifyInputPackets inputPackets) (serverInputBuffers server)
    newServer = server 
      { serverGameHistory = take 60 $ newServerGame : (serverGameHistory server) 
      , serverInputBuffers = scrapedInputBuffers
      }
    (latestInputs, scrapedInputBuffers) = scrapeInputBuffers updatedInputBuffers
    newServerGame = stepServerGame latestInputs (serverGameHistory server)

mapifyInputPackets :: [(PlayerID, PlayerInput)] -> M.Map PlayerID [PlayerInput]
mapifyInputPackets = 
  let
    buildMap accumMap (pid, inputs) = M.alter f pid accumMap
      where
        f (Just xs) = Just $  inputs : xs
        f  Nothing  = Just $ [inputs]
  in 
  foldl' buildMap M.empty

updateBuffer :: PlayerID -> [PlayerInput] -> [PlayerInput] -> Maybe [PlayerInput]
updateBuffer _ a b = Just $ b ++ a 

scrapeInputBuffers :: M.Map PlayerID [PlayerInput] -> (M.Map PlayerID PlayerInput, M.Map PlayerID [PlayerInput])
scrapeInputBuffers = undefined




title :: Picture
title = scale 0.1 0.1 . color fgColor . text $ ""

renderSim :: Simulation -> Picture
renderSim sim = pictures [title, server sim, clients sim]
  where
    server = renderServer . head . serverGameHistory . simServer
    clients = pictures . zipWith3 renderClient [0,1..] (map clientPlayerID (simClients sim)) . map clientGame . simClients

viewBoxSize :: Float
viewBoxSize = 350

viewBoxPadding :: Float
viewBoxPadding = 10

renderServer :: Game -> Picture
renderServer = scale viewBoxSize viewBoxSize . renderServerGame

renderClient :: Int -> PlayerID -> Game -> Picture
renderClient i pid = translate xOffset 0 . scale viewBoxSize viewBoxSize . renderClientGame pid
  where xOffset | i == 0 = -viewBoxSize - viewBoxPadding
                | i == 1 =  viewBoxSize + viewBoxPadding
                | otherwise = undefined