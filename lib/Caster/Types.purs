module Caster.Types where

import Data.Foldable (any)
import Data.Map (Map, lookup)
import Data.Maybe (Maybe)
import Prelude ((<), (>=), (*), (+), id)

type Coord =
    { x :: Int
    , y :: Int
    }

type Level =
    { tileMap :: Map Int Int
    , tileSet :: Array Tile
    , width   :: Int
    , height  :: Int
    }

tileAt :: Level -> Coord -> Maybe Int
tileAt level coord = lookup index level.tileMap
    where index = coord.y * level.width + coord.x

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
rotateLeft East  = South
rotateLeft West  = North

rotateRight :: Direction -> Direction
rotateRight North = East
rotateRight South = West
rotateRight East  = South
rotateRight West  = North

type Player =
    { position  :: Coord
    , direction :: Direction
    }

type Viewport =
    { width  :: Number
    , height :: Number
    }
