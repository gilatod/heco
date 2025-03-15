module Heco.Effectful.InternalTimeStream where

import Heco.Data.TimePhase (TimePhase, ImmanantContent)

import Effectful (Effect, Eff, (:>))
import Effectful.TH (makeEffect)
import Effectful.Dispatch.Dynamic (HasCallStack)

import Data.Vector (Vector)
import Data.Vector qualified as V

data InternalTimeStream :: Effect where
    ProgressUrimpression :: InternalTimeStream m TimePhase
    EnrichUrimpression :: Vector ImmanantContent -> InternalTimeStream m TimePhase
    GetUrimpression :: InternalTimeStream m TimePhase
    GetRetention :: InternalTimeStream m (Vector TimePhase)
    GetRetentionLength :: InternalTimeStream m Int
    GetRetentionCapacity :: InternalTimeStream m Int

makeEffect ''InternalTimeStream

progressUrimpression_ :: (HasCallStack, InternalTimeStream :> es) => Eff es ()
progressUrimpression_ = progressUrimpression >> pure ()

enrichUrimpression_ :: (HasCallStack, InternalTimeStream :> es) => Vector ImmanantContent -> Eff es ()
enrichUrimpression_ contents = enrichUrimpression contents >> pure ()

enrichUrimpressionSingular :: (HasCallStack, InternalTimeStream :> es) => ImmanantContent -> Eff es TimePhase
enrichUrimpressionSingular content = enrichUrimpression (V.singleton content)

enrichUrimpressionSingular_ :: (HasCallStack, InternalTimeStream :> es) => ImmanantContent -> Eff es ()
enrichUrimpressionSingular_ contents = enrichUrimpressionSingular contents >> pure ()