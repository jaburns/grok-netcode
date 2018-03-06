module Network(
    NetworkState
  , Network
  , createNetwork
  , clientSendPacket, clientReceivePackets
  , serverSendPacket, serverReceivePackets
  , clearPacketQueues
  , runNetwork
) where

import Control.Monad.Trans.State
import System.Random

import Input(TaggedInputs)
import Game(Game)

type Packet a = (Float, a)

type ClientPacket = Packet TaggedInputs
type ServerPacket = Packet Game

data NetworkState = NetworkState'
  { netRNG           :: StdGen
  , netClientPackets :: [ClientPacket]
  , netServerPackets :: [ServerPacket]
  -- have lists for packets in transit and packets ready to be read,
  }

type Network = State NetworkState

buildPacket :: a -> Network (Packet a)
buildPacket contents = do
    net <- get
    let (rand, newRNG) = random $ netRNG net
    put $ net { netRNG = newRNG }
    return (getLatency rand, contents)
  where
    getLatency norm = 0.1 + 0.1 * norm

createNetwork :: IO (Network ())
createNetwork = do
    rng <- newStdGen
    return . put $ NetworkState' rng [] []

clientSendPacket :: TaggedInputs -> Network ()
clientSendPacket contents = do
    newPacket <- buildPacket contents
    modify (\net -> net { netClientPackets = newPacket : (netClientPackets net) })

clientReceivePackets :: Network [Game]
clientReceivePackets = undefined

serverSendPacket :: Game -> Network ()
serverSendPacket contents = do
    newPacket <- buildPacket contents
    modify (\net -> net { netServerPackets = newPacket : (netServerPackets net) })

serverReceivePackets :: Network [TaggedInputs]
serverReceivePackets = undefined

clearPacketQueues :: Network ()
clearPacketQueues = undefined

runNetwork :: Float -> NetworkState -> (Float -> Network ()) -> NetworkState
runNetwork dt net update = execState update' net
  where
    update' = do
        modify (updatePackets dt)
        update dt
     -- TODO collectElapsedPackets and place them in to the ready lists to be consumed by the receive functions

updatePackets :: Float -> NetworkState -> NetworkState
updatePackets dt net = net 
    { netServerPackets = map (elapsePacket dt) (netServerPackets net)
    , netClientPackets = map (elapsePacket dt) (netClientPackets net) 
    }
  where
    elapsePacket dt (latency, x) = (latency - dt, x)