module Input(
    TaggedInputs
  , GameInputs
  , KeyMapping
  , defaultGameInputs
  , inputUp, inputLeft, inputRight
  , taggedInputs, taggedUUID, taggedFrame
  , standardMapping, wasdMapping
  , updateInputsWithEvent
  , tagInputs
) where

import Data.UUID(UUID)
import Graphics.Gloss.Interface.Pure.Game
import System.Random

data TaggedInputs = TaggedInputs'
  { taggedInputs :: GameInputs
  , taggedUUID   :: UUID
  , taggedFrame  :: Int
  }

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
evaluateMapping mapping key pressed inputs
    | key == mapUp    mapping = inputs { inputUp    = pressed }
    | key == mapLeft  mapping = inputs { inputLeft  = pressed }
    | key == mapRight mapping = inputs { inputRight = pressed }
    | otherwise = inputs

updateInputsWithEvent :: KeyMapping -> Event -> GameInputs -> GameInputs
updateInputsWithEvent mapping (EventKey key dir _ _) = evaluateMapping mapping key (dir == Down)
updateInputsWithEvent _ _ = id

tagInputs :: RandomGen g => Int -> GameInputs -> g -> (TaggedInputs, g)
tagInputs frame inputs inRNG = (TaggedInputs' inputs uuid frame, newRNG)
  where (uuid, newRNG) = random inRNG