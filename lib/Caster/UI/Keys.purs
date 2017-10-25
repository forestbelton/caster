module Caster.Keys
    ( KEYS
    , initKeys
    , checkKey
    ) where

import Control.Monad.Eff
import Data.Function.Uncurried (Fn2, runFn2)
import Prelude (($), Unit, unit, pure)

foreign import data KEYS :: Effect

foreign import initKeysImpl :: Fn2 Unit String Unit

initKeys :: forall eff. String -> Eff (keys :: KEYS | eff) Unit
initKeys id = pure $ runFn2 initKeysImpl unit id

foreign import checkKey :: forall eff. Int -> Eff (keys :: KEYS | eff) Boolean