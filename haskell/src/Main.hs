{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Main(main) where

import Control.Lens
import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

data InputState = InputState 
  { _up    :: Bool
  , _left  :: Bool
  , _right :: Bool
  }

data GameState = GameState 
  { _inputs :: InputState
  , _time   :: Float
  }

makeLenses ''InputState
makeLenses ''GameState

initialInputs :: InputState
initialInputs = InputState False False False

initialState :: GameState
initialState = GameState initialInputs 0

window :: Display
window = InWindow "Grok Netcode" (1280, 720) (10, 10)

render :: GameState -> Picture
render state = circle $ state^.time

handleEvents :: Event -> GameState -> GameState
handleEvents (EventKey key dir _ _) = over inputs updateInputs
  where
    updateInputs = case mapKeyToInputLens key of
      Just lens -> over lens $ const $ dir == Down
      Nothing -> id

    mapKeyToInputLens (SpecialKey KeyUp)    = Just up
    mapKeyToInputLens (SpecialKey KeyLeft)  = Just left
    mapKeyToInputLens (SpecialKey KeyRight) = Just right
    mapKeyToInputLens _ = Nothing

handleEvents _ = id

update :: Float -> GameState -> GameState
update _ state = if state^.inputs.up then over time (+ 1) state else state

main :: IO ()
main = play window white 30 initialState render handleEvents update