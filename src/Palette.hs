module Palette (
    bgColor, fgColor, oneColor, twoColor
) where


import Numeric
import Graphics.Gloss.Interface.Pure.Game


hexColor :: String -> Color
hexColor (a:b:c:d:e:f:[]) = 
    makeColorI (get [a,b]) (get [c,d]) (get [e,f]) 255
  where 
    get = fst . head . readHex


bgColor :: Color
bgColor = hexColor "252A34"

fgColor :: Color
fgColor = hexColor "EAEAEA"

oneColor :: Color 
oneColor = hexColor "08D9D6"

twoColor :: Color
twoColor = hexColor "FF2E63"