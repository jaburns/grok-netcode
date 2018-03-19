module Input(
    AllInputs
  , PlayerInput
  , InputID 
  , KeyMapping(..)
  , newInputs, emptyPlayerInput
  , inputUp, inputLeft, inputRight, inputID
  , updateInputsWithEvent
  , readPlayerInput
) where


import Data.UUID
import qualified Data.Map.Strict as M
import Graphics.Gloss.Interface.Pure.Game
import System.Random


type InputID = UUID

newtype AllInputs = AllInputs' (M.Map KeyMapping PlayerInput)

data PlayerInput = PlayerInput'
    { inputID    :: InputID
    , inputUp    :: Bool
    , inputLeft  :: Bool
    , inputRight :: Bool
    , inputLaser :: Bool
    }
  deriving (Show)

data KeyMapping = WASD | Arrows
  deriving (Show, Eq, Ord)

data KeyMappingKeys = KeyMappingKeys'
    { mapUp    :: Key
    , mapLeft  :: Key
    , mapRight :: Key
    , mapLaser :: Key
    }


newInputs :: AllInputs
newInputs = AllInputs' $ M.fromList [(WASD, emptyPlayerInput), (Arrows, emptyPlayerInput)]


emptyPlayerInput :: PlayerInput
emptyPlayerInput = PlayerInput' nil False False False False


evaluateMapping :: Key -> Bool -> KeyMapping -> PlayerInput -> PlayerInput
evaluateMapping key pressed mapping inputs
    | key == mapUp    (getKeys mapping) = inputs { inputUp    = pressed }
    | key == mapLeft  (getKeys mapping) = inputs { inputLeft  = pressed }
    | key == mapRight (getKeys mapping) = inputs { inputRight = pressed }
    | key == mapLaser (getKeys mapping) = inputs { inputLaser = pressed }
    | otherwise = inputs
  where
    getKeys WASD = KeyMappingKeys' 
        (Char 'w') 
        (Char 'a') 
        (Char 'd') 
        (Char 's')
    getKeys Arrows = KeyMappingKeys' 
        (SpecialKey KeyUp) 
        (SpecialKey KeyLeft) 
        (SpecialKey KeyRight) 
        (SpecialKey KeyDown)


updateInputsWithEvent :: Event -> AllInputs -> AllInputs
updateInputsWithEvent (EventKey key dir _ _) (AllInputs' inputs) = 
    AllInputs' $ M.mapWithKey (evaluateMapping key (dir == Down)) inputs
updateInputsWithEvent _ x = x


readPlayerInput :: RandomGen g => AllInputs -> KeyMapping -> g -> (PlayerInput, g)
readPlayerInput (AllInputs' allInp) mapping rng = 
    (result, rng')
  where
    (resultID, rng') = random rng
    result = (allInp M.! mapping) { inputID = resultID }