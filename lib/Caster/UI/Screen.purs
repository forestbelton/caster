module Caster.UI.Screen
    ( Screen
    , getScreen
    , drawScreen
    , module ScreenData
    ) where

import Control.Monad.Eff (Eff)
import Data.Foldable (sequence_)
import Data.Maybe (Maybe(..))
import Graphics.Canvas (CANVAS, getCanvasElementById, getContext2D, Context2D)
import Prelude (pure, bind, ($), (<$>), unit, Unit, map, (<<<))

import Caster.UI.RayCaster (ScreenData, Line, castRays)
import Caster.UI.RayCaster (ScreenData) as ScreenData

newtype Screen = Screen Context2D

getScreen :: forall eff. String -> Eff (canvas :: CANVAS | eff) (Maybe Screen)
getScreen id = do
    maybeCanvas <- getCanvasElementById id
    case maybeCanvas of
        Nothing     -> pure Nothing
        Just canvas -> (Just <<< Screen) <$> getContext2D canvas

drawScreen :: forall eff. Screen -> ScreenData -> Eff (canvas :: CANVAS | eff) Unit
drawScreen (Screen ctx) dat = sequence_ $ map (drawLine ctx) lines
    where lines = castRays dat

drawLine :: forall eff. Context2D -> Line -> Eff (canvas :: CANVAS | eff) Unit
drawLine ctx line = pure unit