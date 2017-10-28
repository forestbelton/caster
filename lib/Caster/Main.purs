module Caster.Main
    ( main
    ) where

import Caster.Keys (KEYS, checkKey, initKeys, Key(..), toKeyCode)
import Caster.Types (tileAt, Direction(..), moveDirection, flipDirection, rotateLeft, rotateRight)
import Caster.UI.Screen (Screen, ScreenData, getScreen, drawScreen)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, error)
import Control.Monad.Eff.Ref (REF, Ref, newRef, modifyRef, readRef)
import Data.Maybe (Maybe(..))
import Graphics.Canvas (CANVAS)
import Prelude (($), discard, bind, pure, Unit, unit, not)

foreign import requestAnimationFrame :: forall eff a. Eff eff a -> Eff eff Unit

-------------------------------------------------------
--                     SEED DATA
-------------------------------------------------------

worldMap :: Array (Array Int)
worldMap =
  [ [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]
  , [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
  , [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
  , [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
  , [1,0,0,0,0,0,2,2,2,2,2,0,0,0,0,3,0,3,0,3,0,0,0,1]
  , [1,0,0,0,0,0,2,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,1]
  , [1,0,0,0,0,0,2,0,0,0,2,0,0,0,0,3,0,0,0,3,0,0,0,1]
  , [1,0,0,0,0,0,2,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,1]
  , [1,0,0,0,0,0,2,2,0,2,2,0,0,0,0,3,0,3,0,3,0,0,0,1]
  , [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
  , [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
  , [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
  , [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
  , [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
  , [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
  , [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
  , [1,4,4,4,4,4,4,4,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
  , [1,4,0,4,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
  , [1,4,0,0,0,0,5,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
  , [1,4,0,4,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
  , [1,4,0,4,4,4,4,4,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
  , [1,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
  , [1,4,4,4,4,4,4,4,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
  , [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]
  ]

initialData :: ScreenData
initialData =
    { player:
        { position: { x: 12, y: 12 }
        , direction: North
        }
    , level:
        { tileMap: worldMap
        , tileSet: []
        , width: 24
        , height: 24
        }
    , viewport: { width: 500.0, height: 400.0 }
    }

-------------------------------------------------------

type App eff a = Eff (keys :: KEYS, canvas :: CANVAS, console :: CONSOLE, ref :: REF | eff) a

main :: forall eff. App eff Unit
main = do _ <- initKeys
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

updateInput :: forall eff. Ref ScreenData -> App eff Unit
updateInput dataRef = do
    screenData <- readRef dataRef
    let player = screenData.player

    downKey <- checkKey $ toKeyCode Down
    if downKey
        then let position' = moveDirection player.direction player.position in
             case tileAt screenData.level position' of
                Just _  -> modifyRef dataRef $ \d -> d { player { position = position' }}
                Nothing -> pure unit
        else pure unit

    upKey <- checkKey $ toKeyCode Up
    if upKey
        then let position' = moveDirection (flipDirection player.direction) player.position in
             case tileAt screenData.level position' of
                Just _  -> modifyRef dataRef $ \d -> d { player { position = position' }}
                Nothing -> pure unit
        else pure unit

    leftKey <- checkKey $ toKeyCode Left
    if leftKey
        then let direction' = rotateLeft player.direction in
             modifyRef dataRef $ \d -> d { player { direction = direction' }}
        else pure unit

    rightKey <- checkKey $ toKeyCode Right
    if rightKey
        then let direction' = rotateRight player.direction in
             modifyRef dataRef $ \d -> d { player { direction = direction' }}
        else pure unit
