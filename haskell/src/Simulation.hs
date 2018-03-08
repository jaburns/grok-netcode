module Simulation(
    Simulation
  , newSimulation
  , simClients, clientGame
  , handleSimEvent
  , updateSim
  , getRenderableGames
) where

import Control.Monad
import Control.Monad.Trans.State
import Data.UUID(UUID)
import Graphics.Gloss.Interface.Pure.Game
import System.Random

import Network (Network, newNetwork, updateNetwork, clientReceivePackets, clientSendPackets, 
    serverReceivePackets, serverSendPackets, clearPacketQueues)
import Game (Game, newGame, stepGame, gameTime)
import Input (GameInputs, TaggedInputs, KeyMapping, defaultGameInputs, wasdMapping, updateInputsWithEvent, tagInputs, taggedInputs)

type ServerPayload = Game
type ClientPayload = TaggedInputs
type SimNetwork = Network ClientPayload ServerPayload

data Simulation = Simulation'
  { simRandom  :: StdGen
  , simInputs  :: GameInputs
  , simClients :: [Client]
  , simServer  :: Server
  , simNetwork :: SimNetwork
  }

data Client = Client'
  { clientUID          :: UUID
  , clientKeyMapping   :: KeyMapping
  , clientInputHistory :: [GameInputs]
  , clientGame         :: Game
  }

data Server = Server'
  { serverGame :: Game
  }

newClient :: RandomGen g => g -> (Client, g)
newClient rng = (Client' uuid wasdMapping [] (newGame (0,0)), newRNG)
  where (uuid, newRNG) = random rng

newSimulation :: StdGen -> Simulation
newSimulation rng = Simulation' newRNG' defaultGameInputs clients server net
  where
    genClients = runState . replicateM 1 $ state newClient
    (clients, newRNG) = genClients rng
    (newRNG', netRNG) = split newRNG
    server = Server' (newGame (0,0))
    net = newNetwork netRNG

handleSimEvent :: Event -> Simulation -> Simulation
handleSimEvent event sim = sim { simInputs = updateInputsWithEvent wasdMapping event (simInputs sim) }

zipInputsWithTags :: RandomGen g => Int -> GameInputs -> g -> ([TaggedInputs], g)
zipInputsWithTags count inputs = runState . replicateM count . state $ tagInputs (-1) inputs

updateSim :: Float -> Simulation -> Simulation
updateSim dt = execState $ do
    modify $ \sim -> sim { simNetwork = updateNetwork dt (simNetwork sim) }
    modify updateServer
    modify updateClients
    modify $ \sim -> sim { simNetwork = clearPacketQueues (simNetwork sim) }
  where
    updateClients sim = let
        clients = simClients sim
        packets = clientReceivePackets (simNetwork sim)
        (newInputs, newRNG) = zipInputsWithTags (length clients) (simInputs sim) (simRandom sim)
        runClient inputs client = updateClient inputs packets client
        ranClients = zipWith runClient newInputs clients
      in sim 
        { simClients = map snd ranClients 
        , simRandom = newRNG
        , simNetwork = clientSendPackets (concat . map fst $ ranClients) (simNetwork sim)
        }
    updateServer sim = let
        server = simServer sim
        packets = serverReceivePackets (simNetwork sim)
        newServer = updateServer' packets server 
      in sim 
        { simServer = newServer
        , simNetwork = serverSendPackets [serverGame newServer] (simNetwork sim)
        }

updateServer' :: [ClientPayload] -> Server -> Server --  -> ([ServerPayload], Server)
updateServer' inputPackets = execState $ do
    mapM_ updateServerWithPackets inputPackets
  where 
    updateServerWithPackets :: TaggedInputs -> State Server ()
    updateServerWithPackets packet = do
        modify (\server -> server { serverGame = stepGame (taggedInputs packet) (serverGame server) })

updateClient :: TaggedInputs -> [ServerPayload] -> Client -> ([ClientPayload], Client)
updateClient inputs [] client = ([inputs], client)
updateClient inputs (game:_) client = ([inputs], client { clientGame = latestGame game (clientGame client) })
  where
    latestGame new old = if gameTime new >= gameTime old then new else old

getRenderableGames :: Simulation -> [Game]
getRenderableGames (Simulation' _ _ clients (Server' sgame) _) = [sgame] ++ cgames
  where cgames = map clientGame clients