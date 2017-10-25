module Caster.Main
    ( main
    ) where

import Graphics.Canvas (CANVAS)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, error)
import Control.Monad.Eff.Ref (REF, Ref, newRef, modifyRef, readRef)
import Data.Array ((..), zip)
import Data.Maybe (Maybe(..))
import Data.Map as M
import Data.Tuple (Tuple(..))
import Prelude (($), discard, bind, pure, (*), (+), Unit, unit)

import Caster.Keys (KEYS, checkKey, initKeys)
import Caster.Types (Direction(..), rotateRight, rotateLeft)
import Caster.UI.Screen (Screen, ScreenData, getScreen, drawScreen)

foreign import requestAnimationFrame :: forall eff a. Eff eff a -> Eff eff Unit

-------------------------------------------------------
--                     SEED DATA
-------------------------------------------------------

worldMap :: Array (Array Int)
worldMap =
    [ [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]
    , [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,4,4,4,4,4,4,1]
    , [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,4,1]
    , [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,4,0,4,4,0,4,1]
    , [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,4,0,4,1]
    , [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,4,0,4,1]
    , [1,0,0,0,2,2,2,2,2,0,0,0,0,0,0,0,4,0,5,0,4,0,4,1]
    , [1,0,0,0,2,0,0,0,2,0,0,0,0,0,0,0,4,0,0,0,4,0,4,1]
    , [1,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,4,4,4,4,4,0,4,1]
    , [1,0,0,0,2,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
    , [1,0,0,0,2,2,2,2,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
    , [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
    , [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
    , [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
    , [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
    , [1,0,0,0,3,0,3,0,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
    , [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
    , [1,0,0,0,3,0,0,0,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
    , [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
    , [1,0,0,0,3,0,3,0,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
    , [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
    , [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
    , [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
    , [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]
    ]

mapTiles :: Array (Tuple Int Int)
mapTiles = do
    Tuple y row <- zip (0..23) worldMap
    Tuple x cell <- zip (0..23) row
    pure $ Tuple (y * 24 + x) cell

initialData :: ScreenData
initialData =
    { player:
        { position: { x: 12, y: 12 }
        , direction: West
        }
    , level:
        { tileMap: M.fromFoldable mapTiles
        , tileSet: []
        , width: 24
        , height: 24
        }
    , viewport: { width: 500.0, height: 400.0 }
    }

-------------------------------------------------------

type App eff a = Eff (keys :: KEYS, canvas :: CANVAS, console :: CONSOLE, ref :: REF | eff) a

main :: forall eff. App eff Unit
main = do _ <- initKeys "canvas"
          screenData <- newRef initialData
          maybeScreen <- getScreen "canvas"
          case maybeScreen of
              Nothing     -> error "could not find screen at #canvas"
              Just screen -> requestAnimationFrame $ loop screen screenData

loop :: forall eff. Screen -> Ref ScreenData -> App eff Unit
loop screen dataRef = do
    updateInput dataRef
    screenData <- readRef dataRef
    drawScreen screen screenData

data Key
    = Left
    | Up
    | Right
    | Down
    | SomeKey Int

toKeyCode :: Key -> Int
toKeyCode Left  = 37
toKeyCode Up    = 38
toKeyCode Right = 39
toKeyCode Down  = 40
toKeyCode (SomeKey x) = x

updateInput :: forall eff. Ref ScreenData -> App eff Unit
updateInput dataRef = do
    leftKey <- checkKey $ toKeyCode Left
    rightKey <- checkKey $ toKeyCode Right
    if rightKey
        then modifyRef dataRef $ \d -> d { player { direction = rotateRight d.player.direction } }
        else if leftKey
            then modifyRef dataRef $ \d -> d { player { direction = rotateLeft d.player.direction } }
            else pure unit