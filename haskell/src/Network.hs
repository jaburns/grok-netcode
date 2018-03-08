module Network(
    Network
  , newNetwork
  , updateNetwork
  , clientSendPackets, clientReceivePackets
  , serverSendPackets, serverReceivePackets
  , clearPacketQueues
) where

import Control.Monad.Trans.State
import System.Random

type Packet a = (Float, a)

data Network client server = Network'
  { netRNG                 :: StdGen
  , netLatency             :: (Float, Float)
  , netClientPackets       :: [Packet client]
  , netServerPackets       :: [Packet server]
  , netClientReadyPayloads :: [client]
  , netServerReadyPayloads :: [server]
  }

newNetwork :: StdGen -> Network a b
newNetwork rng = Network' rng (0.05, 0.025) [] [] [] [] 

updateNetwork :: Float -> Network a b -> Network a b
updateNetwork dt = execState $ do
    modify $ updatePackets dt
    modify movePacketsToReady

updatePackets :: Float -> Network a b -> Network a b
updatePackets dt net = net
  { netServerPackets = map elapsePacket (netServerPackets net)
  , netClientPackets = map elapsePacket (netClientPackets net) 
  }
  where elapsePacket (t, x) = (t - dt, x)

movePacketsToReady :: Network a b -> Network a b
movePacketsToReady (Network' rng lat as bs outAs outBs) = 
    Network' rng lat (remaining as) (remaining bs) (outAs ++ ready as) (outBs ++ ready bs)
  where
    remaining = filter ((> 0) . fst)
    ready = map snd . filter ((<= 0) . fst)

clientSendPackets :: [a] -> Network a b -> Network a b
clientSendPackets payloads = execState $ do
    newPackets <- mapM buildPacket payloads
    modify (\net -> net { netClientPackets = netClientPackets net ++ newPackets })

clientReceivePackets :: Network a b -> [b]
clientReceivePackets = netServerReadyPayloads

serverSendPackets :: [b] -> Network a b -> Network a b
serverSendPackets payload = execState $ do
    newPackets <- mapM buildPacket payload
    modify (\net -> net { netServerPackets = netServerPackets net ++ newPackets })

serverReceivePackets :: Network a b -> [a]
serverReceivePackets = netClientReadyPayloads

clearPacketQueues :: Network a b -> Network a b
clearPacketQueues net = net { netClientReadyPayloads = [], netServerReadyPayloads = [] }
 
buildPacket :: p -> State (Network a b) (Packet p)
buildPacket payload = do
    net <- get
    let (rand, newRNG) = random $ netRNG net
    put $ net { netRNG = newRNG }
    return (getLatency (netLatency net) rand, payload)
  where
    getLatency (base, var) norm = base + var * norm