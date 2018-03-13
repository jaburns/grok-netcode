module Input(
    AllInputs
  , GameInputs
  , InputID 
  , KeyMapping(..)
  , newInputs
  , inputUp, inputLeft, inputRight, inputID
  , updateInputsWithEvent
  , readGameInputs
) where


import Data.UUID(UUID, nil)
import Graphics.Gloss.Interface.Pure.Game
import qualified Data.Map.Strict as M
import System.Random


type InputID = UUID
newtype AllInputs = AllInputs' (M.Map KeyMapping GameInputs)

data GameInputs = GameInputs'
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
newInputs = AllInputs' $ M.fromList [(WASD, newGameInputs), (Arrows, newGameInputs)]

newGameInputs :: GameInputs
newGameInputs = GameInputs' nil False False False

getKeys :: KeyMapping -> KeyMappingKeys
getKeys WASD = KeyMappingKeys' (Char 'w') (Char 'a') (Char 'd')
getKeys Arrows = KeyMappingKeys' (SpecialKey KeyUp) (SpecialKey KeyLeft) (SpecialKey KeyRight)

evaluateMapping :: Key -> Bool -> KeyMapping -> GameInputs -> GameInputs
evaluateMapping key pressed mapping inputs
    | key == mapUp    (getKeys mapping) = inputs { inputUp    = pressed }
    | key == mapLeft  (getKeys mapping) = inputs { inputLeft  = pressed }
    | key == mapRight (getKeys mapping) = inputs { inputRight = pressed }
    | otherwise = inputs

updateInputsWithEvent :: Event -> AllInputs -> AllInputs
updateInputsWithEvent (EventKey key dir _ _) (AllInputs' inputs) = 
    AllInputs' $ M.mapWithKey (evaluateMapping key (dir == Down)) inputs
updateInputsWithEvent _ x = x

readGameInputs :: RandomGen g => AllInputs -> KeyMapping -> g -> (GameInputs, g)
readGameInputs (AllInputs' allInp) mapping rng = (result, rng')
  where 
    result = (allInp M.! mapping) { inputID = resultID }
    (resultID, rng') = random rng