module Render(
    renderSim
) where

import Graphics.Gloss

import Game (Game, gameTime, gameAngle, gamePos)
import Simulation (Simulation, simGames)

shipRadius :: Float
shipRadius = 15

translatePt :: Point -> Picture -> Picture
translatePt (x, y) = translate x y

rotateNegRads :: Float -> Picture -> Picture
rotateNegRads x = rotate (-x * 180 / pi)

renderGame :: Point -> Game -> Picture
renderGame (x,y) game = translatePt (gamePos game) $ rotateNegRads (gameAngle game) $ lineLoop [ 
    (-0.707 * shipRadius, -0.707 * shipRadius)
  , (shipRadius, 0)
  , (-0.707 * shipRadius,  0.707 * shipRadius)
  ]

renderSim :: Simulation -> Picture
renderSim = pictures . map (renderGame (0,0)) . simGames