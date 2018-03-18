module Game(
    Game
  , PlayerID
  , Ship
  , newGame
  , renderClientGame
  , renderServerGame
  , gameFrame
  , addPlayerToGame
  , stepServerGame
  , predictClientGame
) where


import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Map.Merge.Strict as M
import Data.Maybe
import Data.UUID(UUID)
import Graphics.Gloss
import System.Random

import Input (PlayerInput, InputID, inputLeft, inputRight, inputUp, inputID)
import Palette (oneColor, twoColor, fgColor)
import Utils (randomUnitAndList, clamp)


type PlayerID = UUID

data HitStatus = NotHit | Predicted | Confirmed

data Ship = Ship'
    { shipLatestInputID :: InputID
    , shipColor         :: Color
    , shipPos           :: Point
    , shipAngle         :: Float
    , shipHitStatus     :: HitStatus
    , shipHitCooldown   :: Int
    , shipGunCooldown   :: Int
    }

data Laser = Laser'
    { laserOwner      :: PlayerID
    , laserSource     :: Point
    , laserAngle      :: Float
    , laserFramesLeft :: Int
    }

data Game = Game'
    { gameFrame  :: Int
    , gameShips  :: M.Map PlayerID Ship
    , gameLasers :: [Laser]
    }


speed :: Float
speed = 2.5 / 350

shipRadius :: Float
shipRadius = 15 / 350

newGame :: Game
newGame = Game' 0 M.empty []


stepShip :: PlayerInput -> Ship -> Ship
stepShip inputs (Ship' _ col (x, y) angle a b c) = 
    Ship' (inputID inputs) col pos' angle' a b c
  where
    angle' = angle + 0.05 * turn

    turn | inputLeft  inputs =  1
         | inputRight inputs = -1
         | otherwise         =  0

    pos' | inputUp inputs = inBounds (x + speed * cos angle', y + speed * sin angle')
         | otherwise      = (x, y)

    inBounds (ax, ay) = let f = clamp (shipRadius - 0.5) (0.5 - shipRadius) in (f ax, f ay)


stepShips :: M.Map PlayerID PlayerInput -> M.Map PlayerID Ship -> M.Map PlayerID Ship
stepShips = M.merge 
    M.dropMissing 
    M.preserveMissing 
    (M.zipWithMaybeMatched (\_ a b -> Just $ stepShip a b))


addPlayerToGame :: RandomGen g => g -> Game -> (PlayerID, Game, g)
addPlayerToGame rng game = (pid, game', rng')
  where
    ((pid, a:b:c:[]), rng') = randomUnitAndList 3 rng

    color' = if length (gameShips game) `mod` 2 == 0 then oneColor else twoColor

    game' = 
        game 
        { gameShips = M.insert 
            pid 
            (Ship' pid color' (a - 0.5, b - 0.5) (2 * pi * c) NotHit 0 0)
            (gameShips game) 
        }


stepServerGame :: M.Map PlayerID PlayerInput -> [Game] -> Game
stepServerGame _ [] = error "Can't step empty game state history for server."
stepServerGame inputs (game:_) = 
    game 
    { gameFrame = (gameFrame game) + 1
    , gameShips = stepShips inputs (gameShips game)
    }


predictClientGame :: PlayerID -> [PlayerInput] -> Game -> Game
predictClientGame pid inputs game = fromMaybe game $ do
    ship <- (gameShips game) M.!? pid
    index <- findIndex ((== shipLatestInputID ship) . inputID) inputs

    let predictedShip = foldr stepShip ship $ take index inputs

    return $ 
        game 
        { gameShips = M.adjust (const predictedShip) pid (gameShips game) 
        }


renderWindow :: Color -> String -> Picture
renderWindow winColor title = color winColor $ pictures
    [ rectangleWire 1 1
    , drawTitle title 
    ]
  where
    drawTitle = translate (-0.5) (0.5 + 0.01)
        . scale (0.15 / 350) (0.15 /350)
        . color winColor 
        . text 


renderServerGame :: Game -> Picture
renderServerGame (Game' _ ships _) = 
    pictures $ (renderWindow fgColor "Server") : (map drawShip . M.elems $ ships)


renderClientGame :: PlayerID -> Game -> Picture
renderClientGame pid (Game' _ ships _) = 
    pictures $ (renderWindow playerColor "Client") : map drawShip (M.elems ships)
  where 
    playerColor = fromMaybe fgColor $ do 
        ship <- ships M.!? pid
        return $ shipColor ship


drawShip :: Ship -> Picture
drawShip (Ship' _ c (x,y) angle _ _ _) = 
    color c . translate x y $ rotate (-angle * 180 / pi) $ lineLoop
        [ (-0.707 * shipRadius, -0.707 * shipRadius)
        , (shipRadius, 0)
        , (-0.707 * shipRadius,  0.707 * shipRadius)
        ]