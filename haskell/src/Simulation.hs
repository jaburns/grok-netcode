module Simulation(
    Simulation
  , newSimulation
  , handleSimEvent
  , updateSim
  , renderSim
) where

import Control.Monad
import Control.Monad.Trans.State
import qualified Data.Map.Strict as M
import Graphics.Gloss.Interface.Pure.Game
import System.Random

import Network (Network, newNetwork, updateNetwork, clientReceivePackets, clientSendPackets, 
    serverReceivePackets, serverSendPackets, clearPacketQueues)
import Game (Game, PlayerID, newGame, stepGame, gameTime, renderGame)
import Input (GameInputs, TaggedInputs, KeyMapping, defaultGameInputs, wasdMapping, 
    updateInputsWithEvent, tagInputs, taggedInputs)


type ServerPacket = Game
type ClientPacket = (PlayerID, TaggedInputs)

type SimNetwork = Network ClientPacket ServerPacket

data Simulation = Simulation'
  { simRandom  :: StdGen
  , simInputs  :: GameInputs
  , simClients :: [Client]
  , simServer  :: Server
  , simNetwork :: SimNetwork
  }

data Client = Client'
  { clientPlayerID     :: PlayerID
  , clientKeyMapping   :: KeyMapping
  , clientInputHistory :: [GameInputs]
  , clientGame         :: Game
  }

data Server = Server'
  { serverGameHistory  :: [Game]
  , serverInputBuffers :: M.Map PlayerID TaggedInputs
  }


newSimulation :: StdGen -> Simulation
newSimulation rng = Simulation' newRNG' defaultGameInputs clients server net
  where
    genClients = runState . replicateM 2 $ state newClient
    (clients, newRNG) = genClients rng
    (newRNG', netRNG) = split newRNG
    server = Server' [(newGame (0,0))] M.empty
    net = newNetwork netRNG

handleSimEvent :: Event -> Simulation -> Simulation
handleSimEvent event sim = sim { simInputs = updateInputsWithEvent wasdMapping event (simInputs sim) }

updateSim :: Float -> Simulation -> Simulation
updateSim dt = execState $ do
    modify $ \sim -> sim { simNetwork = updateNetwork dt (simNetwork sim) }
    modify updateServerInSimulation
    modify updateClientsInSimulation
    modify $ \sim -> sim { simNetwork = clearPacketQueues (simNetwork sim) }


newClient :: RandomGen g => g -> (Client, g)
newClient rng = (Client' uuid wasdMapping [] (newGame (0,0)), newRNG)
  where (uuid, newRNG) = random rng

updateClientsInSimulation :: Simulation -> Simulation
updateClientsInSimulation sim = sim
  { simClients = map snd ranClients 
  , simRandom = newRNG
  , simNetwork = clientSendPackets (concat . map fst $ ranClients) (simNetwork sim)
  }
  where
    packets = clientReceivePackets (simNetwork sim)
    (newInputs, newRNG) = zipInputsWithTags (length (simClients sim)) (simInputs sim) (simRandom sim)
    runClient inputs client = updateClient inputs packets client
    ranClients = zipWith runClient newInputs (simClients sim)

zipInputsWithTags :: RandomGen g => Int -> GameInputs -> g -> ([TaggedInputs], g)
zipInputsWithTags count inputs = runState . replicateM count . state $ tagInputs (-1) inputs

updateClient :: TaggedInputs -> [ServerPacket] -> Client -> ([ClientPacket], Client)
updateClient inputs [] client = ([(clientPlayerID client, inputs)], client)
updateClient inputs (game:_) client = ([(clientPlayerID client, inputs)], client { clientGame = latestGame game (clientGame client) })
  where latestGame new old = if gameTime new >= gameTime old then new else old


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
    newServer = getNewServer server
    getNewServer = execState $ do
      mapM_ updateServerWithPacket inputPackets
    updateServerWithPacket (_, inputs) = do
        modify (\serv -> serv { serverGameHistory = [stepGame (taggedInputs inputs) (head . serverGameHistory $ server)] })


renderSim :: Simulation -> Picture
renderSim sim = pictures $ renderServer (head . serverGameHistory . simServer $ sim) : zipWith renderClient [0,1..] (map clientGame $ simClients sim)

viewBoxSize :: Float
viewBoxSize = 350

viewBoxPadding :: Float
viewBoxPadding = 10

renderServer :: Game -> Picture
renderServer = scale viewBoxSize viewBoxSize . renderGame

renderClient :: Int -> Game -> Picture
renderClient i = translate xOffset 0 . scale viewBoxSize viewBoxSize . renderGame
  where xOffset | i == 0 = -viewBoxSize - viewBoxPadding
                | i == 1 =  viewBoxSize + viewBoxPadding
                | otherwise = undefined