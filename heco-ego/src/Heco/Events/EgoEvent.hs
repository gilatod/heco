module Heco.Events.EgoEvent where

import Heco.Data.Message (Message, ToolCall, ToolResponse)
import Heco.Data.Immanant.Terminal (TerminalId)

import Data.Vector (Vector)
import Data.Text (Text)

data EgoEvent
    = OnEgoInteractionStarted
    | OnEgoToolUsed ToolCall ToolResponse
    | OnEgoReply TerminalId Text
    | OnEgoInteractionCompleted Message
    | OnEgoInputMessagesGenerated (Vector Message)