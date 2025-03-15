module Heco.Effectful.Ego where

import Heco.Data.Noema (NoemaCategory, NoemaId, Noema)
import Heco.Data.Embedding (Embedding)

import Effectful (Effect, Eff)
import Effectful.TH (makeEffect)

import Data.Vector (Vector)
import Data.Text (Text)
import Data.Vector.Internal.Check (HasCallStack)
import Effectful.Internal.Effect ((:>))

data Ego :: Effect where
    InteractEgo :: m a -> Ego m a
    InjectMemory :: Ego m ()

    PresentiateNoema :: Noema -> Ego m ()
    CreateNoema :: NoemaCategory -> Text -> Ego m Noema
    SetNoema :: Noema -> Ego m ()
    GetNoema :: NoemaId -> Ego m Noema
    FindNoemata :: Embedding -> Ego m (Vector Noema)
    DeleteNoema :: NoemaId -> Ego m ()

makeEffect ''Ego

createNoema_ :: (HasCallStack, Ego :> es)
    => NoemaCategory -> Text -> Eff es ()
createNoema_ c t = createNoema c t >> pure ()