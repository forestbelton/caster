module Caster.Main
    ( main
    ) where

import Graphics.Canvas (CANVAS)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, error)
import Data.Array ((..), zip)
import Data.Maybe (Maybe(..))
import Data.Map as M
import Data.Tuple
import Prelude (($), bind, pure, (*), (+), Unit)

import Caster.Types (Direction(..))
import Caster.UI.Screen (ScreenData, getScreen, drawScreen)

foreign import requestAnimationFrame :: forall eff a. Eff eff a -> Eff eff Unit

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

mapTiles :: Array (Tuple Int Int)
mapTiles = do
    Tuple y row <- zip (0..23) worldMap
    Tuple x cell <- zip (0..23) row
    pure $ Tuple (y * 24 + x) cell

initialData :: ScreenData
initialData =
    { player:
        { position: { x: 12, y: 12 }
        , direction: North
        }
    , level:
        { tileMap: M.fromFoldable mapTiles
        , tileSet: []
        , width: 24
        , height: 24
        }
    , viewport: { width: 500.0, height: 400.0 }
    }

main :: forall eff. Eff (canvas :: CANVAS, console :: CONSOLE | eff) Unit
main = do maybeScreen <- getScreen "canvas"
          case maybeScreen of
              Nothing     -> error "could not find screen at #canvas"
              Just screen -> requestAnimationFrame $ drawScreen screen initialData