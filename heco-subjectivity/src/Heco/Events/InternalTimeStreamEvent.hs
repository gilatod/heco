module Heco.Events.InternalTimeStreamEvent where

import Heco.Data.TimePhase (TimePhase, SomeImmanantContent)
import Data.Vector (Vector)

data InternalTimeStreamEvent
    = OnTimePhaseEnriched TimePhase (Vector SomeImmanantContent)
    | OnTimePhaseRetented TimePhase
    | OnTimePhaseLost TimePhase
    deriving (Show)