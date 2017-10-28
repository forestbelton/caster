module Caster.UI.Screen
    ( Screen
    , getScreen
    , drawScreen
    , module ScreenData
    ) where

import Control.Monad.Eff (Eff)
import Data.Foldable (sequence_)
import Data.Maybe (Maybe(..))
import Graphics.Canvas
import Prelude (pure, bind, ($), (<$>), unit, Unit, map, (<<<), (-))

import Caster.UI.Color (toRGB)
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
drawScreen (Screen ctx) dat = do
    _ <- setFillStyle "black" ctx
    _ <- fillRect ctx { x: 0.0, y: 0.0, w: dat.viewport.width, h: dat.viewport.height }
    sequence_ $ map (drawLine dat.viewport.height ctx) lines
    where lines = castRays dat

drawLine :: forall eff. Number -> Context2D -> Line -> Eff (canvas :: CANVAS | eff) Unit
drawLine height ctx line = do
    -- Draw ceiling
    _ <- setStrokeStyle "#746C73" ctx
    _ <- strokePath ctx $ do
        _ <- moveTo ctx line.x 0.0
        _ <- lineTo ctx line.x line.start
        closePath ctx

    -- Draw wall
    _ <- setStrokeStyle (toRGB line.color) ctx
    _ <- strokePath ctx $ do
        _ <- moveTo ctx line.x line.start
        _ <- lineTo ctx line.x line.end
        closePath ctx

    -- Draw ground
    _ <- setStrokeStyle "#A98F6E" ctx
    _ <- strokePath ctx $ do
        _ <- moveTo ctx line.x line.end
        _ <- lineTo ctx line.x (height - 1.0)
        closePath ctx

    pure unit