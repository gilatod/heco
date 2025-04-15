module Heco.Events.InternalTimeStreamEvent where

import Heco.Data.TimePhase (TimePhase, AnyImmanantContent)
import Data.Vector (Vector)

data InternalTimeStreamEvent
    = OnTimePhaseEnriched TimePhase (Vector AnyImmanantContent)
    | OnTimePhaseRetented TimePhase
    | OnTimePhaseLost TimePhase