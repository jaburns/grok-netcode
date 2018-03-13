module Game(
    Game
  , PlayerID
  , Ship
  , newGame
  , renderGame
  , gameTime
  , addPlayerToGame
  , stepServerGame
) where

import Control.Monad
import Control.Monad.Trans.State
import qualified Data.Map.Strict as M
import Data.UUID(UUID)
import Graphics.Gloss
import System.Random

import Input (GameInputs, inputLeft, inputRight, inputUp)
import Palette (oneColor, twoColor)

type PlayerID = UUID

data Ship = Ship'
  { shipID    :: PlayerID
  , shipPos   :: Point
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

newGame :: Game
newGame = Game' 0 []

updateShip :: GameInputs -> Ship -> Ship
updateShip inputs (Ship' id (x,y) angle) = Ship' id newPos newAngle 
  where
    newAngle = angle + 0.05 * turn
    turn | inputLeft  inputs =  1
         | inputRight inputs = -1
         | otherwise         =  0
    newPos | inputUp inputs = (x + speed * cos newAngle, y + speed * sin newAngle)
           | otherwise      = (x, y)

stepShips :: M.Map PlayerID GameInputs -> [Ship] -> [Ship]
stepShips inputs = map stepShip
  where stepShip ship = case inputs M.!? (shipID ship) of
          Just inp -> updateShip inp ship
          Nothing -> ship

addPlayerToGame :: RandomGen g => g -> Game -> (PlayerID, Game, g)
addPlayerToGame rng game = (newID, game { gameShips = newShip : (gameShips game) }, newRNG')
  where
    newShip = Ship' newID (a - 0.5, b - 0.5) (2 * pi * c)
    (a:b:c:[], newRNG) = runState (replicateM 3 (state random)) rng
    (newID, newRNG') = random newRNG

stepServerGame :: M.Map PlayerID GameInputs -> [Game] -> Game
stepServerGame inputs historicalGames = stepGame (head historicalGames)
  where stepGame game = game 
          { gameTime = (gameTime game) + 1
          , gameShips = stepShips inputs (gameShips game)
          }

stepClientPredictedGame :: PlayerID -> GameInputs -> Game -> Game
stepClientPredictedGame pid inputs game = undefined


renderGame :: Game -> Picture
renderGame (Game' _ ships) = pictures $ (rectangleWire 1 1) : zipWith drawShip [oneColor, twoColor] ships

drawShip :: Color -> Ship -> Picture
drawShip c (Ship' _ (x,y) angle) = color c . translate x y $ rotate (-angle * 180 / pi) $ lineLoop [ 
    (-0.707 * shipRadius, -0.707 * shipRadius)
  , (shipRadius, 0)
  , (-0.707 * shipRadius,  0.707 * shipRadius)
  ]