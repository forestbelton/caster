module Caster.Types where

import Data.Array
import Data.Foldable (any)
import Data.Tuple
import Data.Map
import Prelude ((<), (>=), id)

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

outOfBounds :: Coord -> Level -> Boolean
outOfBounds coord level = any id
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
