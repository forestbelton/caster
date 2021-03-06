module Caster.UI.RayCaster
    ( ScreenData(..)
    , Line(..)
    , castRays
    , playerDir
    ) where

import Data.Array ((..))
import Data.Int (toNumber, floor)
import Data.Maybe (Maybe(..), maybe)
import Math (sqrt)
import Prelude (id, map, max, min, negate, ($), (*), (+), (-), (/), (/=), (<), (<<<))

import Caster.Types (Coord, Direction(..), Level, Player, Viewport, outOfBounds, tileAt)
import Caster.Math.Linear as L
import Caster.UI.Color (Color, blackColor, darken)

foreign import signum :: Number -> Number

type ScreenData =
    { player   :: Player
    , level    :: Level
    , viewport :: Viewport
    }

type Line =
    { x     :: Number
    , start :: Number
    , end   :: Number
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
    East  -> { x: 0.0,   y: -0.66 }
    West  -> { x: 0.0,   y:  0.66 }

playerDir :: Player -> L.V2
playerDir p = case p.direction of
    North -> { x: 0.0,  y: 1.0   }
    South -> { x: 0.0,  y: -1.0  }
    East  -> { x: 1.0,  y: 0.0   }
    West  -> { x: -1.0,  y: 0.0  }

-- | `ray` computes the ray from the player to screen column x
ray :: ScreenData -> Number -> Ray
ray screen x =
    { position: { x: toNumber player.x, y: toNumber player.y }
    , direction: L.add dir (L.scale plane cameraX)
    }
    where dir     = playerDir screen.player
          plane   = playerPlane screen.player
          cameraX = 2.0 * x / screen.viewport.width - 1.0
          player  = screen.player.position

castRays :: ScreenData -> Array Line
castRays screen = map (castRay screen <<< toNumber) xs
    where xs = 0..(floor screen.viewport.width - 1)

castRay :: ScreenData -> Number -> Line
castRay screen x = case collision of
    Nothing -> { x: x, start: 0.0, end: 0.0, color: blackColor }
    Just collision' -> getLine screen x collision'
    where r         = ray screen x
          collision = findCollision state
          deltaDist = getDeltaDist r
          state     =
              { level: screen.level
              , ray: r
              , position: screen.player.position
              , step: L.map (floor <<< signum) r.direction
              , sideDist: getSideDist r deltaDist
              , deltaDist: deltaDist
              , side: NotOnSide
              }

getLine :: ScreenData -> Number -> Collision -> Line
getLine screen x coll =
    { x: x
    , start: drawStart
    , end: drawEnd
    , color: darkenedColor
    }
    where viewHeight = screen.viewport.height
          lineHeight = toNumber $ floor $ viewHeight / coll.wallDistance
          drawStart = max 0.0 $ (viewHeight - lineHeight) / 2.0
          drawEnd = min (viewHeight - 1.0) $ (lineHeight + viewHeight) / 2.0
          color = getColor $ (maybe 0 id $ tileAt screen.level $ coll.position)
          darkenedColor = case coll.side of
                              OnSide -> darken color
                              NotOnSide -> color

getColor :: Int -> Color
getColor 1 = { red: 255, green: 0, blue: 0 }
getColor 2 = { red: 0, green: 0, blue: 255 }
getColor 3 = { red: 0, green: 255, blue: 0 }
getColor 4 = { red: 255, green: 255, blue: 255 }
getColor _ = blackColor

type SearchState =
    { level     :: Level
    , ray       :: Ray
    , step      :: Coord
    , deltaDist :: L.V2
    , position  :: Coord
    , side      :: Side
    , sideDist  :: L.V2
    }

data Side = OnSide | NotOnSide

type Collision =
    { position     :: Coord
    , wallDistance :: Number
    , side         :: Side
    }

findCollision :: SearchState -> Maybe Collision
findCollision state = if outOfBounds state.level state.position
    then Nothing
    else let state' = updateSearch state in
        if foundWall state'
            then let s    = state'.side
                     pos  = state'.position
                     dist = case s of
                                OnSide -> ((toNumber pos.y - state'.ray.position.y) + (1.0 - toNumber state'.step.y) / 2.0)
                                    / state'.ray.direction.y
                                NotOnSide -> ((toNumber pos.x - state'.ray.position.x) + (1.0 - toNumber state'.step.x) / 2.0)
                                    / state'.ray.direction.x
            in Just { position: pos, side: s, wallDistance: dist }
            else findCollision state'

updateSearch :: SearchState -> SearchState
updateSearch state = if state.sideDist.x < state.sideDist.y
    then moveHorizontally state
    else moveVertically state

-- TODO: Cleanup
moveHorizontally :: SearchState -> SearchState
moveHorizontally state = state { position = position, sideDist = sideDist, side = NotOnSide }
    where position = { x: state.position.x + state.step.x, y: state.position.y }
          sideDist = { x: state.sideDist.x + state.deltaDist.x, y: state.sideDist.y }

moveVertically :: SearchState -> SearchState
moveVertically state = state { position = position, sideDist = sideDist, side = OnSide }
    where position = { x: state.position.x, y: state.position.y + state.step.y }
          sideDist = { x: state.sideDist.x, y: state.sideDist.y + state.deltaDist.y }

foundWall :: SearchState -> Boolean
foundWall state = maybe 0 id (tileAt state.level state.position) /= 0

getDeltaDist :: Ray -> L.V2
getDeltaDist r = { x: deltaDistX, y: deltaDistY }
    where deltaDistX = sqrt $ 1.0 + (r.direction.y * r.direction.y) / (r.direction.x * r.direction.x)
          deltaDistY = sqrt $ 1.0 + (r.direction.x * r.direction.x) / (r.direction.y * r.direction.y)

getSideDist :: Ray -> L.V2 -> L.V2
getSideDist r deltaDist = { x: x, y: y }
    where x = if r.direction.x < 0.0 then 0.0 else deltaDist.x
          y = if r.direction.y < 0.0 then 0.0 else deltaDist.y
