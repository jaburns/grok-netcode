module Main(
    main
) where

import Graphics.Gloss.Interface.Pure.Game

import Render(renderGames)
import Simulation(Simulation, newRandomSimulation, handleSimEvent, updateSim, getRenderableGames)

window :: Display
window = InWindow "Grok Netcode" (1280, 720) (10, 10)

render :: Simulation -> Picture
render = renderGames . getRenderableGames 

main :: IO ()
main = do
    simState <- newRandomSimulation
    play window white 30 simState render handleSimEvent updateSim