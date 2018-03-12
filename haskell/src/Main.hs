module Main(
    main
) where

import Graphics.Gloss.Interface.Pure.Game
import System.Random

import Simulation(newSimulation, handleSimEvent, updateSim, renderSim)

window :: Display
window = InWindow "Grok Netcode" (1280, 720) (10, 10)

main :: IO ()
main = do
    rng <- getStdGen
    play window white 60 (newSimulation rng) renderSim handleSimEvent updateSim