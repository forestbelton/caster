module Caster.Math.Linear
    ( V2(..)
    , M22(..)
    , map
    , add
    , sub
    , scale
    , mult
    ) where

import Prelude as P

-- [ x y ]
type V2 =
    { x :: Number
    , y :: Number
    }

-- [ a b ]
-- [ c d ]
type M22 =
    { a :: Number
    , b :: Number
    , c :: Number
    , d :: Number
    }

map :: forall a. (Number -> a) -> V2 -> { x :: a, y :: a }
map f v = { x: f v.x, y: f v.y }

zipWith :: (Number -> Number -> Number) -> V2 -> V2 -> V2
zipWith f v w = { x: f v.x w.x, y: f v.y w.y }

add :: V2 -> V2 -> V2
add = zipWith P.add

sub :: V2 -> V2 -> V2
sub = zipWith P.sub

scale :: V2 -> Number -> V2
scale v k = { x: P.mul v.x k, y: P.mul v.x k }

mult :: V2 -> M22 -> V2
mult v m = { x: x', y: y' }
    where x' = P.add (P.mul v.x m.a) (P.mul v.y m.c)
          y' = P.add (P.mul v.x m.b) (P.mul v.y m.d)