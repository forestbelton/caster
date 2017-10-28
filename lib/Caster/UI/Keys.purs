module Caster.Keys
    ( KEYS
    , initKeys
    , checkKey
    , Key(..)
    , toKeyCode
    ) where

import Control.Monad.Eff
import Prelude (($), Unit, unit, pure)

foreign import data KEYS :: Effect

foreign import initKeysImpl :: Unit -> Unit

initKeys :: forall eff. Eff (keys :: KEYS | eff) Unit
initKeys = pure $ initKeysImpl unit

foreign import checkKey :: forall eff. Int -> Eff (keys :: KEYS | eff) Boolean

data Key
    = Left
    | Up
    | Right
    | Down
    | SomeKey Int

toKeyCode :: Key -> Int
toKeyCode Left  = 37
toKeyCode Up    = 38
toKeyCode Right = 39
toKeyCode Down  = 40
toKeyCode (SomeKey x) = x
