module Input(
    AllInputs
  , PlayerInput
  , InputID 
  , KeyMapping(..)
  , newInputs
  , inputUp, inputLeft, inputRight, inputID
  , updateInputsWithEvent
  , readPlayerInput
) where


import Data.UUID(UUID, nil)
import Graphics.Gloss.Interface.Pure.Game
import qualified Data.Map.Strict as M
import System.Random


type InputID = UUID
newtype AllInputs = AllInputs' (M.Map KeyMapping PlayerInput)

data PlayerInput = PlayerInput'
  { inputID    :: InputID
  , inputUp    :: Bool
  , inputLeft  :: Bool
  , inputRight :: Bool
  }

data KeyMapping = WASD | Arrows
  deriving (Eq, Ord)

data KeyMappingKeys = KeyMappingKeys'
  { mapUp    :: Key
  , mapLeft  :: Key
  , mapRight :: Key
  }


newInputs :: AllInputs
newInputs = AllInputs' $ M.fromList [(WASD, newPlayerInput), (Arrows, newPlayerInput)]

newPlayerInput :: PlayerInput
newPlayerInput = PlayerInput' nil False False False

getKeys :: KeyMapping -> KeyMappingKeys
getKeys WASD = KeyMappingKeys' (Char 'w') (Char 'a') (Char 'd')
getKeys Arrows = KeyMappingKeys' (SpecialKey KeyUp) (SpecialKey KeyLeft) (SpecialKey KeyRight)

evaluateMapping :: Key -> Bool -> KeyMapping -> PlayerInput -> PlayerInput
evaluateMapping key pressed mapping inputs
    | key == mapUp    (getKeys mapping) = inputs { inputUp    = pressed }
    | key == mapLeft  (getKeys mapping) = inputs { inputLeft  = pressed }
    | key == mapRight (getKeys mapping) = inputs { inputRight = pressed }
    | otherwise = inputs

updateInputsWithEvent :: Event -> AllInputs -> AllInputs
updateInputsWithEvent (EventKey key dir _ _) (AllInputs' inputs) = 
    AllInputs' $ M.mapWithKey (evaluateMapping key (dir == Down)) inputs
updateInputsWithEvent _ x = x

readPlayerInput :: RandomGen g => AllInputs -> KeyMapping -> g -> (PlayerInput, g)
readPlayerInput (AllInputs' allInp) mapping rng = (result, rng')
  where 
    result = (allInp M.! mapping) { inputID = resultID }
    (resultID, rng') = random rng