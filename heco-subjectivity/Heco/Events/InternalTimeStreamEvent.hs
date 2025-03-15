module Heco.Events.InternalTimeStreamEvent where

import Heco.Data.TimePhase (TimePhase, ImmanantContent)
import Data.Vector (Vector)

data InternalTimeStreamEvent
    = TimePhaseEnrichedEvent TimePhase (Vector ImmanantContent)
    | TimePhaseRetentedEvent TimePhase
    | TimePhaseLostEvent TimePhase