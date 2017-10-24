module Caster.Main
    ( main
    ) where

import Graphics.Canvas (CANVAS)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, error)
import Data.Maybe (Maybe(..))
import Data.Map as M
import Prelude (($), bind, Unit)

import Caster.Types (Direction(..))
import Caster.UI.Screen (ScreenData, getScreen, drawScreen)

foreign import requestAnimationFrame :: forall eff a. Eff eff a -> Eff eff Unit

initialData :: ScreenData
initialData =
    { player:
        { position: { x: 12, y: 12 }
        , direction: West
        }
    , level:
        { tileMap: M.empty
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