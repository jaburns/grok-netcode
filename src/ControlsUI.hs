module ControlsUI (
    Controls
  , newControls
  , handleControlsEvent
  , renderControls
) where


import Graphics.Gloss.Interface.Pure.Game

import Utils(clamp)
import Palette(fgColor, oneColor)


data Controls = Controls'
    { ctrlBasePingSlider :: Slider
    , ctrlVaryPingSlider :: Slider
    , ctrlHistorySlider  :: Slider
    }

data SliderState = 
    Default | Hovering | Holding
  deriving (Eq)

data Slider = Slider'
    { sliderPosition   :: Point
    , sliderValue      :: Float
    , sliderState      :: SliderState
    , sliderWidth      :: Float
    , sliderLabel      :: String
    , sliderLowerBound :: Float
    , sliderUpperBound :: Float
    }


newControls :: Controls
newControls = Controls'
    { ctrlBasePingSlider = Slider' (0, -200) 0 Default 350 "Base Ping"     0 200
    , ctrlVaryPingSlider = Slider' (0, -230) 0 Default 350 "Ping Variance" 0 100
    , ctrlHistorySlider  = Slider' (0, -260) 0 Default 350 "View History"  0 300
    }
    -- TODO Why are slider values initialized to max-value instead of zero?


handleControlsEvent :: Event -> Controls -> Controls
handleControlsEvent event controls =
    controls
    { ctrlBasePingSlider = updateSlider event $ ctrlBasePingSlider controls
    , ctrlVaryPingSlider = updateSlider event $ ctrlVaryPingSlider controls
    , ctrlHistorySlider  = updateSlider event $ ctrlHistorySlider  controls
    }


renderControls :: Controls -> Picture
renderControls (Controls' a b c) = pictures $ map renderSlider [a, b, c]


sliderBoxWidth     =  10 :: Float
sliderBoxHeight    =  20 :: Float
sliderTextSize     = 0.1 :: Float
sliderValueOffsetX =  10 :: Float
sliderValueOffsetY =   5 :: Float
sliderLabelOffsetX = 100 :: Float


updateSlider :: Event -> Slider -> Slider
updateSlider (EventMotion (x, y)) slider = 
    case sliderState slider of
        Holding  -> slider { sliderValue = xPositionToValue slider x }
        Hovering -> slider { sliderState = state' }
        Default  -> slider { sliderState = state' }
  where
    state' = if isHovering slider (x, y) then Hovering else Default
    xPositionToValue (Slider' (sx, _) _ _ width _ _ _) px = clamp 0 1 $ 0.5 - (px - sx) / width

updateSlider (EventKey (MouseButton LeftButton) dir _ (x, y)) slider = 
    slider
    { sliderState = case dir of
        Down -> if isHovering slider (x, y) then Holding  else (sliderState slider)
        Up   -> if isHovering slider (x, y) then Hovering else Default
    }

updateSlider _ slider = slider


isHovering :: Slider -> Point -> Bool
isHovering (Slider' (x, y) val _ width _ _ _) (mx, my) =
       (mx - x') >= -sliderBoxWidth  / 2 && (mx - x') <= sliderBoxWidth  / 2
    && (my - y ) >= -sliderBoxHeight / 2 && (my - y ) <= sliderBoxHeight / 2
  where
    x' = x + width * (0.5 - val)


renderSlider :: Slider -> Picture
renderSlider (Slider' (x, y) val state width label upper lower) =
    translate x y $ pictures  
        [ color boxColor $ translate boxX 0 $ rectangleWire sliderBoxWidth sliderBoxHeight
        , color fgColor $ maybeLine (-width / 2) (boxX - sliderBoxWidth / 2)
        , color fgColor $ maybeLine (boxX + sliderBoxWidth / 2) (width / 2)
        , color fgColor $ drawLabel
        , color fgColor $ drawValue
        ]
  where
    maybeLine xa xb = if xb > xa then line [(xa, 0), (xb, 0)] else blank
    boxX = width * (0.5 - val)
    boxColor = case state of Default -> fgColor
                             _       -> oneColor
    drawValue = translate (sliderValueOffsetX + width / 2) (-sliderValueOffsetY)
        . scale sliderTextSize sliderTextSize . text 
        . show . round $ lower + val * (upper - lower)
    drawLabel = translate (-sliderLabelOffsetX - width / 2) (-sliderValueOffsetY)
        . scale sliderTextSize sliderTextSize . text
        $ label