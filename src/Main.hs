module Main(
    main
) where


import Graphics.Gloss.Interface.Pure.Game
import System.Random

import Simulation(newSimulation, handleSimEvent, updateSim, renderSim)
import Palette(bgColor)


window :: Display
window = InWindow "Grok Netcode" (1280, 720) (50, 50)

main :: IO ()
main = do
    rng <- getStdGen
    play window bgColor 60 (newSimulation rng) renderSim handleSimEvent updateSim