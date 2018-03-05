module Main(
    main
) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Render(renderSim)
import Simulation(defaultSim, handleSimEvent, updateSim)

window :: Display
window = InWindow "Grok Netcode" (1280, 720) (10, 10)

main :: IO ()
main = play window white 30 defaultSim renderSim handleSimEvent updateSim