module Caster.Types where

import Data.Array
import Data.Tuple
import Data.Map

type Level =
    { tileMap :: Map Int Int
    , tileSet :: Array Tile
    , width   :: Int
    , height  :: Int
    }

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
    { position  :: { x :: Int, y :: Int }
    , direction :: Direction
    }

type Viewport =
    { width  :: Number
    , height :: Number
    }
