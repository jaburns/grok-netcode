module Network(
    Network
  , newNetworkFromSeed
  , runNetwork
  , clientSendPacket, clientReceivePackets
  , serverSendPacket, serverReceivePackets
  , clearPacketQueues
) where

import Control.Monad.Trans.State
import System.Random

type Packet a = (Float, a)

data Network a b = Network'
  { netRNG                 :: StdGen
  , netClientPackets       :: [Packet a]
  , netServerPackets       :: [Packet b]
  , netClientReadyPayloads :: [a]
  , netServerReadyPayloads :: [b]
  }

newNetworkFromSeed :: Float -> Network a b
newNetworkFromSeed seed = Network' (read . show $ seed) [] [] [] []

runNetwork :: Float -> Network a b -> Network a b
runNetwork dt = execState $ do
    modify $ updatePackets dt
    modify movePacketsToReady

updatePackets :: Float -> Network a b -> Network a b
updatePackets dt net = net
  { netServerPackets = map elapsePacket (netServerPackets net)
  , netClientPackets = map elapsePacket (netClientPackets net) 
  }
  where
    elapsePacket (t, x) = (t - dt, x)

movePacketsToReady :: Network a b -> Network a b
movePacketsToReady (Network' rng as bs outAs outBs) = 
    Network' rng (remaining as) (remaining bs) ((ready as) ++ outAs) ((ready bs) ++ outBs)
  where
    remaining = filter ((> 0) . fst)
    ready = map snd . filter ((<= 0) . fst)

clientSendPacket :: a -> Network a b -> Network a b
clientSendPacket payload = execState $ do
    newPacket <- buildPacket payload
    modify (\net -> net { netClientPackets = newPacket : (netClientPackets net) })

clientReceivePackets :: Network a b -> [b]
clientReceivePackets = netServerReadyPayloads

serverSendPacket :: b -> Network a b -> Network a b
serverSendPacket payload = execState $ do
    newPacket <- buildPacket payload
    modify (\net -> net { netServerPackets = newPacket : (netServerPackets net) })

serverReceivePackets :: Network a b -> [a]
serverReceivePackets = netClientReadyPayloads

clearPacketQueues :: Network a b -> Network a b
clearPacketQueues net = net { netClientReadyPayloads = [], netServerReadyPayloads = [] }
 
buildPacket :: p -> State (Network a b) (Packet p)
buildPacket payload = do
    net <- get
    let (rand, newRNG) = random $ netRNG net
    put $ net { netRNG = newRNG }
    return (getLatency rand, payload)
  where
    getLatency norm = 0.1 + 0.1 * norm