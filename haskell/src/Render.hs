module Render(
    renderGames
) where

import Graphics.Gloss

import Game (Game, gameAngle, gamePos)

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

drawShip :: Game -> Picture
drawShip game = translatePt (gamePos game) $ rotateNegRads (gameAngle game) $ lineLoop [ 
    (-0.707 * shipRadius, -0.707 * shipRadius)
  , (shipRadius, 0)
  , (-0.707 * shipRadius,  0.707 * shipRadius)
  ]

renderGame :: Point -> Game -> Picture
renderGame pos game = translatePt pos $ pictures [drawShip game, rectangleWire viewBoxSize viewBoxSize]

renderGames :: [Game] -> Picture
renderGames = pictures . map renderGameAt . zip [-viewBoxSize - viewBoxPadding, 0..] . reverse
  where renderGameAt (pos, game) = renderGame (pos, 0) game