module ControlsUI (
    Controls
  , newControls
  , handleControlsEvent
  , renderControls
  , evaluateSlider

  , ctrlBasePingSlider
  , ctrlVaryPingSlider
  , ctrlDropRateSlider
  , ctrlHistorySlider
) where


import Graphics.Gloss.Interface.Pure.Game

import Palette
import Utils


data Controls = Controls'
    { ctrlBasePingSlider :: Slider
    , ctrlVaryPingSlider :: Slider
    , ctrlDropRateSlider :: Slider
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
    { ctrlBasePingSlider = newSlider 0 0 500  50 "Base Ping"
    , ctrlVaryPingSlider = newSlider 1 0 200  30 "Ping Variance"
    , ctrlDropRateSlider = newSlider 2 0 100   0 "Drop Rate"
    , ctrlHistorySlider  = newSlider 3 0 350 350 "View History"
    }
  where
    newSlider i mini maxi val label = 
        Slider' (0, -160 - 30 * i) ((val - mini) / (maxi - mini)) Default 350 label mini maxi


handleControlsEvent :: Event -> Controls -> Controls
handleControlsEvent event controls =
    controls
    { ctrlBasePingSlider = updateSlider event $ ctrlBasePingSlider controls
    , ctrlVaryPingSlider = updateSlider event $ ctrlVaryPingSlider controls
    , ctrlDropRateSlider = updateSlider event $ ctrlDropRateSlider controls
    , ctrlHistorySlider  = updateSlider event $ ctrlHistorySlider  controls
    }


renderControls :: Controls -> Picture
renderControls (Controls' a b c d) = pictures $ map renderSlider [a, b, c, d]


evaluateSlider :: Slider -> Float
evaluateSlider (Slider' _ val _ _ _ lower upper) = lower + val * (upper - lower)


sliderBoxWidth     =  10 :: Float
sliderBoxHeight    =  20 :: Float
sliderTextSize     = 0.1 :: Float
sliderValueOffsetX =  12 :: Float
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
    xPositionToValue (Slider' (sx, _) _ _ width _ _ _) px = clamp 0 1 $ 0.5 + (px - sx) / width

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
    x' = x + width * (val - 0.5)


renderSlider :: Slider -> Picture
renderSlider slider =
    translate x y $ pictures  
        [ color boxColor $ translate boxX 0 $ rectangleWire sliderBoxWidth sliderBoxHeight
        , color fgColor $ maybeLine (-width / 2) (boxX - sliderBoxWidth / 2)
        , color fgColor $ maybeLine (boxX + sliderBoxWidth / 2) (width / 2)
        , color fgColor $ drawLabel
        , color fgColor $ drawValue
        ]
  where
    (Slider' (x, y) val state width label _ _) = slider

    maybeLine xa xb = if xb > xa then line [(xa, 0), (xb, 0)] else blank

    boxX = width * (val - 0.5)

    boxColor = case state of Default -> fgColor
                             _       -> oneColor

    drawValue = translate (sliderValueOffsetX + width / 2) (-sliderValueOffsetY)
        . scale sliderTextSize sliderTextSize . text 
        . show $ sliderRoundValue

    sliderRoundValue :: Int
    sliderRoundValue = round $ evaluateSlider slider

    drawLabel = translate (-sliderLabelOffsetX - width / 2) (-sliderValueOffsetY)
        . scale sliderTextSize sliderTextSize . text
        $ label