module Caster.Main
    ( main
    ) where

import Control.Monad.Eff (Eff)
import Prelude (pure, unit, Unit)

main :: forall eff. Eff eff Unit
main = pure unit
