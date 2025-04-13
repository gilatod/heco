module Heco.Data.Immanant.Terminal where

import Heco.Data.TimePhase (ImmanantContent(..))
import Heco.Data.Message (Message, messageText, ToolResponse(..))

import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB
import Data.Text.Lazy.Builder.Int qualified as TLB
import Data.Aeson.Text qualified as Aeson

data Terminal
    = TerminalChat Int Text
    | TerminalReply Int Message
    | TerminalToolResponse ToolResponse
    | TerminalClose Int
    
instance ImmanantContent Terminal where
    encodeImmanantContent = \case
        TerminalChat _ c -> ["chat", c]
        TerminalReply _ c -> ["reply", messageText c]
        TerminalToolResponse r ->
            let content = TL.toStrict $ Aeson.encodeToLazyText r.content
            in ["tool_response", r.name, content]
        TerminalClose _ -> ["close"]
    getImmanantContentAttributes = \case
        TerminalChat id _ -> [sessionAttr id]
        TerminalReply id _ -> [sessionAttr id]
        TerminalToolResponse _ -> []
        TerminalClose id -> [sessionAttr id]
        where
            sessionAttr id = ("session", numToText id)
            numToText = TL.toStrict . TLB.toLazyText . TLB.decimal