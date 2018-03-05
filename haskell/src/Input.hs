module Input(
    GameInputs
  , defaultGameInputs
  , inputUp, inputLeft, inputRight
  , updateInputsFromEvent
) where

import Graphics.Gloss.Interface.Pure.Game

data GameInputs = GameInputsState
  { inputUp    :: Bool
  , inputLeft  :: Bool
  , inputRight :: Bool
  }

defaultGameInputs :: GameInputs
defaultGameInputs = GameInputsState False False False

updateInputsFromEvent :: Event -> GameInputs -> GameInputs
updateInputsFromEvent (EventKey (SpecialKey key) dir _ _) inputs = case key of
    KeyUp    -> inputs { inputUp    = pressed }
    KeyLeft  -> inputs { inputLeft  = pressed } 
    KeyRight -> inputs { inputRight = pressed }
    _        -> inputs
  where
    pressed = dir == Down
updateInputsFromEvent _ inputs = inputs