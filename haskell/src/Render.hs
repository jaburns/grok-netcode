module Render(
    renderSim
) where

import Graphics.Gloss

import Game (Game, gameTime, gameAngle, gamePos)
import Simulation (Simulation, simGames)

shipRadius :: Float
shipRadius = 15

viewBoxSize :: Float
viewBoxSize = 350

viewBoxPadding :: Float
viewBoxPadding = 10

translatePt :: Point -> Picture -> Picture
translatePt (x, y) = translate x y

rotateNegRads :: Float -> Picture -> Picture
rotateNegRads x = rotate (-x * 180 / pi)

drawBox :: Point -> Point -> Picture
drawBox pos (w, h) = translatePt pos $ rectangleWire w h

drawShip :: Game -> Picture
drawShip game = translatePt (gamePos game) $ rotateNegRads (gameAngle game) $ lineLoop [ 
    (-0.707 * shipRadius, -0.707 * shipRadius)
  , (shipRadius, 0)
  , (-0.707 * shipRadius,  0.707 * shipRadius)
  ]

renderGame :: Point -> Game -> Picture
renderGame pos game = pictures [drawShip game, drawBox pos (viewBoxSize,viewBoxSize)]

renderSim :: Simulation -> Picture
renderSim = pictures . map renderGameAt . zip [-viewBoxSize - viewBoxPadding, 0..] . simGames
  where
    renderGameAt (pos, game) = renderGame (pos, 0) game