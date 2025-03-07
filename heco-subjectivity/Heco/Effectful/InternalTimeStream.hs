module Heco.Effectful.InternalTimeStream where

import Heco.Data.TimePhase (TimePhase, ImmanantContent)

import Effectful (Effect)
import Effectful.TH (makeEffect)

data InternalTimeStream :: Effect where
    ProgressUrimpression :: InternalTimeStream m ()
    EnrichUrimpression :: ImmanantContent -> InternalTimeStream m ()
    Urimpression :: InternalTimeStream m TimePhase
    Retention :: InternalTimeStream m [TimePhase]
    GetRetentionLength :: InternalTimeStream m Int
    GetRetentionCapacity :: InternalTimeStream m Int

makeEffect ''InternalTimeStream