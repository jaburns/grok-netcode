module ControlsUI (
    Controls
  , newControls
  , handleControlsEvent
  , renderControls
) where

import Graphics.Gloss.Interface.Pure.Game

data Controls = Controls'
    { ctrlSomeSlider :: Slider
    }

data SliderState = 
    Default | Hovering | Holding
  deriving (Eq)

data Slider = Slider'
    { sliderPosition :: Point
    , sliderValue    :: Float
    , sliderState    :: SliderState
    }


newControls :: Controls
newControls = Controls'
    { ctrlSomeSlider = Slider' (100, -200) 0 Default
    }


handleControlsEvent :: Event -> Controls -> Controls
handleControlsEvent event controls =
    controls
    { ctrlSomeSlider = updateSlider event (ctrlSomeSlider controls)
    }


renderControls :: Controls -> Picture
renderControls (Controls' slider) = renderSlider slider


updateSlider :: Event -> Slider -> Slider
updateSlider (EventMotion (x, y)) slider = 
    case sliderState slider of
        Holding  -> slider { sliderPosition = (x, y) }
        Hovering -> slider { sliderState = state' }
        Default  -> slider { sliderState = state' }
  where
    state' = if isHovering slider (x, y) then Hovering else Default

updateSlider (EventKey (MouseButton LeftButton) dir _ (x, y)) slider = 
    slider
    { sliderState = case dir of
        Down -> if isHovering slider (x, y) then Holding  else (sliderState slider)
        Up   -> if isHovering slider (x, y) then Hovering else Default
    }

updateSlider _ slider = slider


lineWidth = 350 :: Float
boxWidth  =  10 :: Float
boxHeight =  20 :: Float


positionToValue :: Slider -> Point -> Float
positionToValue (Slider' (sx, sy) _ _) (px, py) = undefined

valueToPosition :: Slider -> Float -> Point
valueToPosition = undefined


isHovering :: Slider -> Point -> Bool
isHovering (Slider' (sx, sy) _ _) (mx, my) = 
       (mx - sx) >= -boxWidth  / 2 && (mx - sx) <= boxWidth  / 2
    && (my - sy) >= -boxHeight / 2 && (my - sy) <= boxHeight / 2


renderSlider :: Slider -> Picture
renderSlider (Slider' (x, y) _ state) = 
    translate x y $ color boxColor $ pictures  
        [ rectangleWire boxWidth boxHeight
        , line [(-lineWidth / 2, 0), (-boxWidth / 2, 0)]
        , line [( lineWidth / 2, 0), ( boxWidth / 2, 0)]
        ]
  where
    boxColor = case state of Default  -> white
                             Hovering -> green
                             Holding  -> red
        


{--
renderControls :: Controls -> Picture
renderControls (Controls' (x, y)) = 
    color white
    $ translate x y 
    $ thickCircle 1 30
--}