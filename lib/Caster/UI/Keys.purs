module Caster.Keys
    ( KEYS
    , initKeys
    , checkKey
    ) where

import Control.Monad.Eff
import Prelude (($), Unit, unit, pure)

foreign import data KEYS :: Effect

foreign import initKeysImpl :: Unit -> Unit

initKeys :: forall eff. Eff (keys :: KEYS | eff) Unit
initKeys = pure $ initKeysImpl unit

foreign import checkKey :: forall eff. Int -> Eff (keys :: KEYS | eff) Boolean