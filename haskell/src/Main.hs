module Main(
    main
) where

import Graphics.Gloss.Interface.Pure.Game

import Render(renderGames)
import Simulation(Simulation, newDefaultSim, handleSimEvent, updateSim)

window :: Display
window = InWindow "Grok Netcode" (1280, 720) (10, 10)

render :: Simulation -> Picture
render sim = renderGames undefined

main :: IO ()
main = do
    simState <- newDefaultSim
    play window white 30 simState render handleSimEvent updateSim