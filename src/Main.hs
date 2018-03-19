module Main(
    main
) where


import Graphics.Gloss.Interface.Pure.Game
import System.Random

import Simulation
import Palette


window :: Display
window = InWindow "Grok Netcode" (1240, 620) (50, 50)

main :: IO ()
main = do
    rng <- getStdGen
    play window bgColor 60 (newSimulation rng) renderSim handleSimEvent updateSim