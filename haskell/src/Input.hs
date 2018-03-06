module Input(
    GameInputs
  , KeyMapping
  , defaultGameInputs
  , inputUp, inputLeft, inputRight
  , standardMapping, wasdMapping
  , updateInputsWithEvent
) where

import Graphics.Gloss.Interface.Pure.Game

data GameInputs = GameInputs'
  { inputUp    :: Bool
  , inputLeft  :: Bool
  , inputRight :: Bool
  }

data KeyMapping = KeyMapping'
  { mapUp    :: Key
  , mapLeft  :: Key
  , mapRight :: Key
  }

defaultGameInputs :: GameInputs
defaultGameInputs = GameInputs' False False False

standardMapping :: KeyMapping
standardMapping = KeyMapping' (SpecialKey KeyUp) (SpecialKey KeyLeft) (SpecialKey KeyRight)

wasdMapping :: KeyMapping
wasdMapping = KeyMapping' (Char 'w') (Char 'a') (Char 'd')

evaluateMapping :: KeyMapping -> Key -> Bool -> GameInputs -> GameInputs
evaluateMapping mapping key pressed inputs =
    if (key == mapUp    mapping) then inputs { inputUp    = pressed } else
    if (key == mapLeft  mapping) then inputs { inputLeft  = pressed } else
    if (key == mapRight mapping) then inputs { inputRight = pressed } else
    inputs

updateInputsWithEvent :: KeyMapping -> Event -> GameInputs -> GameInputs
updateInputsWithEvent mapping (EventKey key dir _ _) = evaluateMapping mapping key (dir == Down)
updateInputsWithEvent _ _ = id