module Game(
    Game
  , gamePos, gameAngle
  , newGame
  , stepGame
) where

import Graphics.Gloss
import Input (GameInputs, inputLeft, inputRight, inputUp)

data Game = GameState
  { gameTime  :: Int
  , gamePos   :: Point
  , gameAngle :: Float
  }

speed :: Float
speed = 5

newGame :: Point -> Game
newGame pos = GameState 0 pos 0

updatePos :: Bool -> Float -> Point -> Point
updatePos False _ = id
updatePos True angle = \(x,y) -> (x + speed * cos angle, y + speed * sin angle)

stepGame :: GameInputs -> Game -> Game
stepGame inputs game = game
  { gameTime = (gameTime game) + 1
  , gameAngle = newAngle
  , gamePos = updatePos (inputUp inputs) newAngle (gamePos game)
  }
  where
    newAngle = gameAngle game + 0.1 * turn
    turn | inputLeft  inputs =  1
         | inputRight inputs = -1
         | otherwise         =  0