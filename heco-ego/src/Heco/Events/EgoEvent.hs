module Heco.Events.EgoEvent where

import Heco.Data.Message (Message, ToolCall, ToolResponse)
import Heco.Data.Immanant.Terminal (TerminalId)
import Heco.Data.TimePhase (TimePhase)

import Data.Vector (Vector)
import Data.Text (Text)

data EgoEvent
    = OnEgoInteractionStarted
    | OnEgoInteractionCompleted Message
    | OnEgoInputMessagesGenerated (Vector Message)
    | OnEgoReply TimePhase TerminalId Text
    | OnEgoToolUsed ToolCall ToolResponse