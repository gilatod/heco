module Heco.Events.InternalTimeStreamEvent where

import Heco.Data.TimePhase (TimePhase, ImmanantContent)

data InternalTimeStreamEvent
    = TimePhaseEnrichedEvent TimePhase ImmanantContent
    | TimePhaseRetentedEvent TimePhase
    | TimePhaseLostEvent TimePhase