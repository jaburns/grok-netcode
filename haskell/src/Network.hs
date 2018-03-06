module Network(
    createNetwork
  , clientSendPacket, serverSendPacket
  , updateNetwork
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
  }

type NetworkM = State NetworkState

buildPacket :: a -> NetworkM (Packet a)
buildPacket contents = do
    net <- get
    let (rand, newRNG) = random $ netRNG net
    put $ net { netRNG = newRNG }
    return (getLatency rand, contents)
  where
    getLatency norm = 0.1 + 0.1 * norm

elapsePacket :: Float -> Packet a -> Packet a
elapsePacket dt (latency, x) = (latency - dt, x)

collectElapsedPackets :: [Packet a] -> ([Packet a], a)
collectElapsedPackets = undefined

createNetwork :: IO (NetworkM ())
createNetwork = do
    rng <- newStdGen
    return . put $ NetworkState' rng [] []

clientSendPacket :: TaggedInputs -> NetworkM ()
clientSendPacket contents = do
    newPacket <- buildPacket contents
    modify (\net -> net { netClientPackets = newPacket : (netClientPackets net) })

serverSendPacket :: Game -> NetworkM ()
serverSendPacket contents = do
    newPacket <- buildPacket contents
    modify (\net -> net { netServerPackets = newPacket : (netServerPackets net) })

updateNetwork :: Float -> NetworkM ()
updateNetwork dt = do
    modify (\net -> net { 
          netServerPackets = map (elapsePacket dt) (netServerPackets net)
        , netClientPackets = map (elapsePacket dt) (netClientPackets net) })
     -- TODO collectElapsedPackets and do something with them