module Palette (
    bgColor, mainColor
) where

import Numeric(readHex)
import Graphics.Gloss.Interface.Pure.Game

hexColor :: String -> Color
hexColor (a:b:c:d:e:f:_) = makeColorI (get [a,b]) (get [c,d]) (get [e,f]) 255
  where get = fst . head . readHex
hexColor _ = undefined

bgColor :: Color
bgColor = hexColor "FAE3D9"

mainColor :: Color 
mainColor = hexColor "FFB6B9"

--B  BBDED6
--C  61C0BF