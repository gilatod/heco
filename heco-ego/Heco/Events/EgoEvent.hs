module Heco.Events.EgoEvent where

import Data.Text (Text)

data EgoEvent
    = OnEgoInteractionStarted
    | OnEgoTaskGenerated Text
    | OnEgoTaskResponded Text
    | OnEgoInteractionCompleted