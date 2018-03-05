module Main(
    main
) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Render(renderSim)
import Simulation(newDefaultSim, handleSimEvent, updateSim)

window :: Display
window = InWindow "Grok Netcode" (1280, 720) (10, 10)

main :: IO ()
main = do
    simState <- newDefaultSim
    play window white 30 simState renderSim handleSimEvent updateSim