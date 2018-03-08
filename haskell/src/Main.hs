module Main(
    main
) where

import Graphics.Gloss.Interface.Pure.Game
import System.Random

import Render(renderGames)
import Simulation(Simulation, newSimulation, handleSimEvent, updateSim, getRenderableGames)

window :: Display
window = InWindow "Grok Netcode" (1280, 720) (10, 10)

render :: Simulation -> Picture
render = renderGames . getRenderableGames 

main :: IO ()
main = do
    rng <- getStdGen
    play window white 60 (newSimulation rng) render handleSimEvent updateSim