module Caster.Types where

import Data.Array ((!!))
import Data.Foldable (any)
import Data.Map (Map, lookup)
import Data.Maybe (Maybe)
import Prelude (bind, (<), (>=), (*), (+), (-), id)

type Coord =
    { x :: Int
    , y :: Int
    }

type Level =
    { tileMap :: Array (Array Int)
    , tileSet :: Array Tile
    , width   :: Int
    , height  :: Int
    }

tileAt :: Level -> Coord -> Maybe Int
tileAt level coord = do
    column <- level.tileMap !! coord.x
    column !! coord.y

outOfBounds :: Level -> Coord -> Boolean
outOfBounds level coord = any id
    [ coord.x < 0
    , coord.x >= level.width
    , coord.y < 0
    , coord.y >= level.height
    ]

-- TODO
data Tile

data Direction
    = North
    | South
    | East
    | West

rotateLeft :: Direction -> Direction
rotateLeft North = West
rotateLeft South = East
rotateLeft East  = North
rotateLeft West  = South

rotateRight :: Direction -> Direction
rotateRight North = East
rotateRight South = West
rotateRight East  = South
rotateRight West  = North

moveDirection :: Direction -> Coord -> Coord
moveDirection North c = { x: c.x, y: c.y - 1 }
moveDirection South c = { x: c.x, y: c.y + 1 }
moveDirection East  c = { x: c.x - 1, y: c.y }
moveDirection West  c = { x: c.x + 1, y: c.y }

type Player =
    { position  :: Coord
    , direction :: Direction
    }

type Viewport =
    { width  :: Number
    , height :: Number
    }
