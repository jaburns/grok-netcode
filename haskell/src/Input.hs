module Input(
    AllInputs
  , GameInputs
  , KeyMapping(..)
  , newInputs
  , inputUp, inputLeft, inputRight, inputFrame
  , updateInputsWithEvent
  , readGameInputs
) where


import Graphics.Gloss.Interface.Pure.Game
import qualified Data.Map.Strict as M


newtype AllInputs = AllInputs' (M.Map KeyMapping GameInputs)

data GameInputs = GameInputs'
  { inputUp    :: Bool
  , inputLeft  :: Bool
  , inputRight :: Bool
  , inputFrame :: Int
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
newGameInputs = GameInputs' False False False (-1)

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

readGameInputs :: AllInputs -> KeyMapping -> Int -> GameInputs
readGameInputs (AllInputs' allInp) mapping frame = (allInp M.! mapping) { inputFrame = frame }