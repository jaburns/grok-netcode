module Game(
    Game
  , PlayerID
  , Ship
  , newGame
  , renderClientGame, renderServerGame
  , gameFrame
  , addPlayerToGame
  , stepServerGame, predictClientGame
) where


import Control.Monad
import Control.Monad.Trans.State
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Map.Merge.Strict as M
import Data.Maybe
import Data.UUID(UUID)
import Graphics.Gloss
import System.Random

import Input (GameInputs, InputID, inputLeft, inputRight, inputUp, inputID)
import Palette (oneColor, twoColor, fgColor)


type PlayerID = UUID

data Ship = Ship'
  { shipLatestInputID :: InputID
  , shipColor         :: Color
  , shipPos           :: Point
  , shipAngle         :: Float
  }

data Game = Game'
  { gameFrame :: Int
  , gameShips :: M.Map PlayerID Ship
  }


speed :: Float
speed = 2.5 / 350

shipRadius :: Float
shipRadius = 15 / 350

newGame :: Game
newGame = Game' 0 M.empty

stepShip :: GameInputs -> Ship -> Ship
stepShip inputs (Ship' _ col (x,y) angle) = Ship' (inputID inputs) col newPos newAngle 
  where
    newAngle = angle + 0.05 * turn
    turn | inputLeft  inputs =  1
         | inputRight inputs = -1
         | otherwise         =  0
    newPos | inputUp inputs = (x + speed * cos newAngle, y + speed * sin newAngle)
           | otherwise      = (x, y)

stepShips :: M.Map PlayerID GameInputs -> M.Map PlayerID Ship -> M.Map PlayerID Ship
stepShips = M.merge (M.dropMissing) (M.preserveMissing) (M.zipWithMaybeMatched f)
  where f _ a b = Just $ stepShip a b


addPlayerToGame :: RandomGen g => g -> Game -> (PlayerID, Game, g)
addPlayerToGame rng game = (newID, game { gameShips = M.insert newID newShip (gameShips game) }, newRNG')
  where
    newShip = Ship' newID col (a - 0.5, b - 0.5) (2 * pi * c)
    (a:b:c:[], newRNG) = runState (replicateM 3 (state random)) rng
    (newID, newRNG') = random newRNG
    col = if length (gameShips game) `mod` 2 == 0 then oneColor else twoColor

stepServerGame :: M.Map PlayerID GameInputs -> [Game] -> Game
stepServerGame inputs historicalGames = stepGame (head historicalGames)
  where stepGame game = game 
          { gameFrame = (gameFrame game) + 1
          , gameShips = stepShips inputs (gameShips game)
          }

predictClientGame :: PlayerID -> [GameInputs] -> Game -> Game
predictClientGame pid inputs game = fromMaybe game $ do
    ship <- (gameShips game) M.!? pid
    index <- findIndex ((== shipLatestInputID ship) . inputID) inputs
    let predictedShip = foldr stepShip ship (take index inputs)
    return $ game { gameShips = M.adjust (const predictedShip) pid (gameShips game) }

renderServerGame :: Game -> Picture
renderServerGame (Game' _ ships) = pictures $ (color fgColor $ rectangleWire 1 1) : (map drawShip . M.elems $ ships)

renderClientGame :: PlayerID -> Game -> Picture
renderClientGame pid (Game' _ ships) = pictures $ (color playerColor $ rectangleWire 1 1) : map drawShip (M.elems ships)
  where playerColor = fromMaybe fgColor $ do 
          ship <- ships M.!? pid
          return $ shipColor ship

drawShip :: Ship -> Picture
drawShip (Ship' _ c (x,y) angle) = color c . translate x y $ rotate (-angle * 180 / pi) $ lineLoop [ 
    (-0.707 * shipRadius, -0.707 * shipRadius)
  , (shipRadius, 0)
  , (-0.707 * shipRadius,  0.707 * shipRadius)
  ]