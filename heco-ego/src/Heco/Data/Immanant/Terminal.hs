module Heco.Data.Immanant.Terminal where

import Heco.Data.TimePhase (ImmanantContent(..))
import Heco.Data.Message (Message, formatMessage)

import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB
import Data.Text.Lazy.Builder.Int qualified as TLB
import Data.Hashable (Hashable)
import Pattern.Cast (Cast(cast))

newtype TerminalId = TerminalId Int
    deriving (Show, Eq, Ord, Enum, Bounded)
    deriving Num via Int
    deriving newtype Hashable

instance Cast TerminalId Int where
    cast (TerminalId id) = id

data Terminal
    = TerminalChat TerminalId Text
    | TerminalReply Message
    deriving (Eq, Show)

instance ImmanantContent Terminal where
    encodeImmanantContent = \case
        TerminalChat _ c -> ["chat", c]
        TerminalReply msg -> ["reply", TL.toStrict $ formatMessage msg]
    getImmanantContentAttributes = \case
        TerminalChat id _ -> [sessionAttr id]
        _ -> []
        where
            sessionAttr id = ("session", numToText id)
            numToText = TL.toStrict . TLB.toLazyText . TLB.decimal . cast @TerminalId @Int