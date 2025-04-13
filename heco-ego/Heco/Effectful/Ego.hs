module Heco.Effectful.Ego where

import Effectful (Effect)
import Effectful.TH (makeEffect)

data Ego :: Effect where
    InteractEgo :: m a -> Ego m a
    InjectMemory :: Ego m ()

makeEffect ''Ego