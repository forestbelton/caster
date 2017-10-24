module Caster.UI.RayCaster
    ( ScreenData(..)
    , Line(..)
    , castRays
    ) where

import Data.Int (toNumber)
import Partial.Unsafe (unsafeCrashWith)
import Prelude

import Caster.Types
import Caster.Math.Linear as L
import Caster.UI.Color

type ScreenData =
    { player   :: Player
    , level    :: Level
    , viewport :: Viewport
    }

type Line =
    { x     :: Int
    , start :: Int
    , end   :: Int
    , color :: Color
    }

type Ray =
    { position  :: L.V2
    , direction :: L.V2
    }

playerPlane :: Player -> L.V2
playerPlane p = case p.direction of
    North -> { x: 0.66,  y: 0.0   }
    South -> { x: -0.66, y: 0.0   }
    East  -> { x: 0.0,   y: 0.66  }
    West  -> { x: 0.0,   y: -0.66 }

playerDir :: Player -> L.V2
playerDir p = case p.direction of
    North -> { x: 0.0,  y: -1.0 }
    South -> { x: 0.0,  y: 1.0  }
    East  -> { x: -1.0, y: 0.0  }
    West  -> { x: 1.0,  y: 0.0  }

-- | `ray` computes the ray from the player to screen column x
ray :: ScreenData -> Int -> Ray
ray screen x =
    { position: { x: toNumber player.x, y: toNumber player.y }
    , direction: L.add dir (L.scale plane cameraX)
    }
    where dir     = playerDir screen.player
          plane   = playerPlane screen.player
          cameraX = 2.0 * toNumber x / screen.viewport.width - 1.0
          player  = screen.player.position

castRays :: ScreenData -> Array Line
castRays screen = unsafeCrashWith "not implemented"