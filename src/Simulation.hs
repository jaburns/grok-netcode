module Simulation(
    Simulation
  , newSimulation
  , handleSimEvent
  , updateSim
  , renderSim
) where


import Control.Monad.Trans.State
import qualified Data.Map.Strict as M
import qualified Data.Map.Merge.Strict as M
import Graphics.Gloss.Interface.Pure.Game
import System.Random

import ControlsUI 
import Game 
import Input 
import Network 


type ServerPacket = Game

type ClientPacket = (PlayerID, PlayerInput)

type SimNetwork = Network ClientPacket ServerPacket

data Simulation = Simulation'
    { simRandom   :: StdGen
    , simInputs   :: AllInputs
    , simClients  :: [Client]
    , simServer   :: Server
    , simNetwork  :: SimNetwork
    , simControls :: Controls
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
newSimulation rng = 
    Simulation' rng2 newInputs [client0, client1] server net newControls
  where
    (id0, game0, rng0) = addPlayerToGame rng  newGame
    (id1, game1, rng1) = addPlayerToGame rng0 game0
    client0 = Client' id0 WASD [] newGame
    client1 = Client' id1 Arrows [] newGame
    server = Server' [game1] M.empty
    (rng2, netRng) = split rng1
    net = newNetwork netRng


handleSimEvent :: Event -> Simulation -> Simulation
handleSimEvent event sim = 
    sim 
    { simInputs   = updateInputsWithEvent event . simInputs $ sim
    , simControls = controls'
    , simNetwork  = network'
    }
  where
    controls' = handleControlsEvent event . simControls $ sim
    network'= configureNetwork 
        (readNetConfigFromControls controls')
        (simNetwork sim)


readNetConfigFromControls :: Controls -> NetworkConfig
readNetConfigFromControls controls = 
    NetworkConfig
    { netConfPing         = evaluateSlider . ctrlBasePingSlider $ controls
    , netConfPingVariance = evaluateSlider . ctrlVaryPingSlider $ controls
    , netConfDropRate     = evaluateSlider . ctrlDropRateSlider $ controls
    }


updateSim :: Float -> Simulation -> Simulation
updateSim dt = execState $ do
    modify $ \sim -> sim { simNetwork = updateNetwork dt (simNetwork sim) }
    modify updateServerInSimulation
    modify updateClientsInSimulation
    modify $ \sim -> sim { simNetwork = clearPacketQueues (simNetwork sim) }


updateClientsInSimulation :: Simulation -> Simulation
updateClientsInSimulation sim = 
    sim
    { simClients = map snd newClientsWithPackets 
    , simRandom = rng'
    , simNetwork = clientSendPackets (concat . map fst $ newClientsWithPackets) (simNetwork sim)
    }
  where
    newClientsWithPackets = 
        zipWith 
        (updateClient . clientReceivePackets . simNetwork $ sim)
        clientInputs 
        (simClients sim)

    (clientInputs, rng') = readInputsForClients (simInputs sim) (simClients sim) (simRandom sim)


readInputsForClients :: AllInputs -> [Client] -> StdGen -> ([PlayerInput], StdGen)
readInputsForClients allInputs clients = 
    runState $ mapM getInput clients
  where
    getInput client = state $ readPlayerInput allInputs (clientKeyMapping client)


updateClient :: [ServerPacket] -> PlayerInput -> Client -> ([ClientPacket], Client)
updateClient serverGames inputs client = 
    ([(clientPlayerID client, inputs)], newClient)
  where
    newClient = 
        client 
        { clientGame = predictClientGame (clientPlayerID client) newInputs' newGame'
        , clientInputHistory = newInputs'
        }

    newInputs' = take 60 $ inputs : clientInputHistory client

    newGame' = case serverGames of 
        (game:_) -> latestGame game (clientGame client)
        []       -> clientGame client

    latestGame new old = if gameFrame new >= gameFrame old then new else old


updateServerInSimulation :: Simulation -> Simulation
updateServerInSimulation sim = 
    sim 
    { simServer = newServer
    , simNetwork = serverSendPackets outgoingPackets (simNetwork sim)
    }
  where
    (outgoingPackets, newServer) = 
        updateServer
        (serverReceivePackets (simNetwork sim))
        (simServer sim) 


updateServer :: [ClientPacket] -> Server -> ([ServerPacket], Server)
updateServer inputPackets server = 
    ([head . serverGameHistory $ newServer], newServer)
  where 
    newServer = 
        server 
        { serverGameHistory = take 60 $ newServerGame : (serverGameHistory server) 
        , serverInputBuffers = scrapedInputBuffers
        }

    (latestInputs, scrapedInputBuffers) = scrapeInputBuffers updatedInputBuffers
    newServerGame = stepServerGame latestInputs (serverGameHistory server)

    updatedInputBuffers = M.merge 
        M.preserveMissing
        M.preserveMissing
        (M.zipWithMaybeMatched (\_ a b -> Just $ b ++ a))
        (mapifyInputPackets inputPackets) 
        (serverInputBuffers server)


mapifyInputPackets :: [(PlayerID, PlayerInput)] -> M.Map PlayerID [PlayerInput]
mapifyInputPackets =
  let
    buildMap (pid, input) = M.alter f pid
      where
        f (Just xs) = Just $  input : xs
        f  Nothing  = Just $ [input]
  in 
    foldr buildMap M.empty


scrapeInputBuffers :: M.Map PlayerID [PlayerInput] 
                   -> (M.Map PlayerID PlayerInput, M.Map PlayerID [PlayerInput])
scrapeInputBuffers buffers = 
    (M.map getFirst buffers, M.map removeFirstUnlessLast buffers)
  where 
    getFirst (x:_) = x
    getFirst    _  = emptyPlayerInput

    removeFirstUnlessLast (_:x:rest) = x : rest
    removeFirstUnlessLast      rest  = rest
  

viewBoxSize :: Float
viewBoxSize = 350

viewBoxPadding :: Float
viewBoxPadding = 10


renderSim :: Simulation -> Picture
renderSim sim = 
    pictures $ [(renderControls . simControls $ sim), games]
  where
    games = translate 0 40 $ pictures $ server : clients
    server = renderServer . head . serverGameHistory . simServer $ sim
    clients = 
        zipWith3 renderClient [0,1..] (map clientPlayerID (simClients sim)) 
        $ map clientGame 
        $ simClients sim


renderServer :: Game -> Picture
renderServer = scale viewBoxSize viewBoxSize . renderServerGame


renderClient :: Int -> PlayerID -> Game -> Picture
renderClient i pid = 
    translate xOffset 0 . scale viewBoxSize viewBoxSize . renderClientGame pid
  where 
    xOffset | i == 0 = -viewBoxSize - viewBoxPadding
            | i == 1 =  viewBoxSize + viewBoxPadding
            | otherwise = undefined