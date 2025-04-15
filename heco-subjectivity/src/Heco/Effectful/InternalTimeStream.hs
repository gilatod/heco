module Heco.Effectful.InternalTimeStream where

import Heco.Data.TimePhase (TimePhase, AnyImmanantContent (AnyImmanantContent), ImmanantContent)

import Effectful (Effect, Eff, (:>))
import Effectful.TH (makeEffect)
import Effectful.Dispatch.Dynamic (HasCallStack)

import Data.Vector (Vector)
import Data.Vector qualified as V

data InternalTimeStream :: Effect where
    ProgressUrimpression :: InternalTimeStream m TimePhase
    EnrichUrimpression :: Vector AnyImmanantContent -> InternalTimeStream m TimePhase
    GetUrimpression :: InternalTimeStream m TimePhase
    GetRetention :: InternalTimeStream m (Vector TimePhase)
    GetRetentionLength :: InternalTimeStream m Int
    GetRetentionCapacity :: InternalTimeStream m Int

makeEffect ''InternalTimeStream

progressUrimpression_ ::
    (HasCallStack, InternalTimeStream :> es) => Eff es ()
progressUrimpression_ = progressUrimpression >> pure ()

enrichUrimpression_ ::
    (HasCallStack, InternalTimeStream :> es)
    => Vector AnyImmanantContent -> Eff es ()
enrichUrimpression_ contents = enrichUrimpression contents >> pure ()

enrichUrimpressionSingle ::
    (HasCallStack, InternalTimeStream :> es, ImmanantContent c)
    => c -> Eff es TimePhase
enrichUrimpressionSingle content =
    enrichUrimpression $ V.singleton $ AnyImmanantContent content

enrichUrimpressionSingle_ ::
    (HasCallStack, InternalTimeStream :> es, ImmanantContent c)
    => c -> Eff es ()
enrichUrimpressionSingle_ content =
    enrichUrimpressionSingle content >> pure ()