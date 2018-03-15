module Network(
    Network
  , newNetwork
  , updateNetwork
  , clientSendPackets, clientReceivePackets
  , serverSendPackets, serverReceivePackets
  , clearPacketQueues
) where


import Control.Monad.Trans.State
import Data.Maybe
import System.Random


type InTransit a = (Float, a)

data Network client server = Network'
  { netRNG                 :: StdGen
  , netLatency             :: (Float, Float)
  , netLossRate            :: Float
  , netClientPackets       :: [InTransit client]
  , netServerPackets       :: [InTransit server]
  , netClientReadyPayloads :: [client]
  , netServerReadyPayloads :: [server]
  }


newNetwork :: StdGen -> Network a b
newNetwork rng = Network' rng (0.05, 0.025) 0 [] [] [] []

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
movePacketsToReady (Network' rng lat loss as bs outAs outBs) =
    Network' rng lat loss (remaining as) (remaining bs) (outAs ++ ready as) (outBs ++ ready bs)
  where
    remaining = filter ((> 0) . fst)
    ready = map snd . filter ((<= 0) . fst)


clientSendPackets :: [a] -> Network a b -> Network a b
clientSendPackets payloads = execState $ do
    maybeNewPackets <- mapM maybeBuildPacket payloads
    let newPackets = catMaybes maybeNewPackets
    modify (\net -> net { netClientPackets = netClientPackets net ++ newPackets })

clientReceivePackets :: Network a b -> [b]
clientReceivePackets = netServerReadyPayloads


serverSendPackets :: [b] -> Network a b -> Network a b
serverSendPackets payload = execState $ do
    maybeNewPackets <- mapM maybeBuildPacket payload
    let newPackets = catMaybes maybeNewPackets
    modify (\net -> net { netServerPackets = netServerPackets net ++ newPackets })

serverReceivePackets :: Network a b -> [a]
serverReceivePackets = netClientReadyPayloads


clearPacketQueues :: Network a b -> Network a b
clearPacketQueues net = net { netClientReadyPayloads = [], netServerReadyPayloads = [] }
 
maybeBuildPacket :: p -> State (Network a b) (Maybe (InTransit p)) -- TODO use StateT Maybe monad transformer
maybeBuildPacket payload = do
    net <- get
    let (rand0, rng0) = random $ netRNG net
    if rand0 < (netLossRate net) then do
        put $ net { netRNG = rng0 }
        return Nothing
    else do
        let (rand1, rng1) = random rng0
        put $ net { netRNG = rng1 }
        return $ Just (getLatency (netLatency net) rand1, payload)
  where
    getLatency (base, var) rand = base + var * rand