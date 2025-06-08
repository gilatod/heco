module Heco.Events.AgentEvent where

import Heco.Data.Message (Message, ToolCall, ToolResponse)
import Heco.Data.TimePhase (TimePhase)
import Heco.Data.Immanant.Terminal (SessionId)

import Data.Vector (Vector)
import Data.Text (Text)

data AgentEvent
    = OnAgentInteractionStarted
    | OnAgentInteractionCompleted Message
    | OnAgentInputMessagesGenerated (Vector Message)
    | OnAgentReply TimePhase SessionId Text
    | OnAgentToolInvoked ToolCall ToolResponse