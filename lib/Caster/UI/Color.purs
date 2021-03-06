module Caster.UI.Color
    ( Color(..)
    , darken
    , toRGB
    , magentaColor
    , blackColor
    ) where

import Data.Monoid ((<>))
import Prelude (div, show)

type Color =
    { red   :: Int
    , green :: Int
    , blue  :: Int
    }

darken :: Color -> Color
darken c = { red: c.red `div` 2
           , green: c.green `div` 2
           , blue: c.blue `div` 2
           }

toRGB :: Color -> String
toRGB c = "rgb(" <> show c.red <> "," <> show c.green <> "," <> show c.blue <> ")"

blackColor :: Color
blackColor = { red: 0, green: 0, blue: 0 }

magentaColor :: Color
magentaColor = { red: 255, green: 0, blue: 255 }