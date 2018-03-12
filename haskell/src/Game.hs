module Game(
    Game
  , PlayerID
  , Ship
  , newGame
  , stepGame
  , renderGame
  , gameTime
) where

import qualified Data.Map.Strict as M
import Data.UUID(UUID)
import Graphics.Gloss

import Input (GameInputs, TaggedInputs, inputLeft, inputRight, inputUp)

type PlayerID = UUID

data Ship = Ship'
  { shipPos   :: Point
  , shipAngle :: Float
  }

data Game = Game'
  { gameTime  :: Int
  , gameShips :: [Ship]
  }

speed :: Float
speed = 2.5 / 350

shipRadius :: Float
shipRadius = 15 / 350

newGame :: Point -> Game
newGame pos = Game' 0 [Ship' pos 0]

updateShip :: GameInputs -> Ship -> Ship
updateShip inputs (Ship' (x,y) angle) = Ship' newPos newAngle 
  where
    newAngle = angle + 0.05 * turn
    turn | inputLeft  inputs =  1
         | inputRight inputs = -1
         | otherwise         =  0
    newPos | inputUp inputs = (x + speed * cos newAngle, y + speed * sin newAngle)
           | otherwise      = (x, y)

stepGame :: GameInputs -> Game -> Game
stepGame inputs game = game
  { gameTime = (gameTime game) + 1
  , gameShips = (map (updateShip inputs) (gameShips game)) 
  }

drawShip :: Ship -> Picture
drawShip (Ship' (x, y) angle) = translate x y $ rotate (-angle * 180 / pi) $ lineLoop [ 
    (-0.707 * shipRadius, -0.707 * shipRadius)
  , (shipRadius, 0)
  , (-0.707 * shipRadius,  0.707 * shipRadius)
  ]

renderGame :: Game -> Picture
renderGame (Game' _ ships) = pictures $ (rectangleWire 1 1) : map drawShip ships

stepServerGame :: M.Map PlayerID TaggedInputs -> [Game] -> Game
stepServerGame inputs historicalGames = undefined

stepClientPredictedGame :: PlayerID -> GameInputs -> Game -> Game
stepClientPredictedGame pid inputs game = undefined