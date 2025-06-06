module Heco.Data.Immanant.Terminal where

import Heco.Data.TimePhase (ImmanantContent(..), TimePhase(..), getImmanantContent)
import Heco.Data.Message (Message, formatMessage)
import Heco.Data.Task (TaskId)
import Heco.Effectful.InternalTimeStream (InternalTimeStream, getLatestRetent)

import Effectful (Eff, (:>))

import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB
import Data.Text.Lazy.Builder.Int qualified as TLB
import Data.Hashable (Hashable)

import Pattern.Cast (Cast(cast))

newtype SessionId = SessionId Int
    deriving (Show, Eq, Ord, Enum, Bounded)
    deriving Num via Int
    deriving newtype Hashable

instance Cast SessionId Int where
    cast (SessionId id) = id

data Terminal
    = TerminalChat SessionId Text
    | TerminalReply TimePhase Message
    | TerminalTaskResponse SessionId TaskId Text
    deriving (Show)

instance ImmanantContent Terminal where
    encodeImmanantContent = \case
        TerminalChat _ c -> ["chat", c]
        TerminalReply _ msg -> ["reply", TL.toStrict $ formatMessage msg]
        TerminalTaskResponse _ _ c -> ["task_response", c]
    getImmanantContentAttributes = \case
        TerminalChat id _ ->
            [("session", numToText $ cast id)]
        TerminalTaskResponse _ id _ ->
            [("id", numToText $ cast id)]
        _ -> []
        where
            numToText :: Int -> Text
            numToText = TL.toStrict . TLB.toLazyText . TLB.decimal

getReplyingTimePhase ::
    ( InternalTimeStream :> es )
    => Eff es (Maybe TimePhase)
getReplyingTimePhase =
    getLatestRetent >>= \case
        Just phase
            | Just (TerminalReply rp _) <- getImmanantContent @Terminal phase -> do
            pure $ Just rp
        _ -> pure Nothing

getReplyingTerminalContent ::
    ( InternalTimeStream :> es )
    => Eff es (Maybe (Terminal, TimePhase))
getReplyingTerminalContent = do
    getReplyingTimePhase >>= \case
        Just phase
            | Just c <- getImmanantContent @Terminal phase -> do
            pure $ Just (c, phase)
        _ -> pure Nothing