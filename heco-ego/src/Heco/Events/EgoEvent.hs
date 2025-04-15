module Heco.Events.EgoEvent where

import Heco.Data.Message (Message)
import Data.Vector (Vector)

data EgoEvent
    = OnEgoInteractionStarted
    | OnEgoInteractionCompleted Message
    | OnEgoInputMessagesGenerated (Vector Message)