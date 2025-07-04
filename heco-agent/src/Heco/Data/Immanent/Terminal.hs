module Heco.Data.Immanent.Terminal where

import Heco.Data.Immanent (Immanent(..))
import Heco.Data.TimePhase (TimePhase(..), getImmanent)
import Heco.Data.Message (Message, formatMessage)
import Heco.Data.Task (TaskId)
import Heco.Effectful.ImmanentTime (ImmanentTime, getLatestRetent)

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

instance Immanent Terminal where
    encode = \case
        TerminalChat _ c -> ["chat", c]
        TerminalReply _ msg -> ["reply", TL.toStrict $ formatMessage msg]
        TerminalTaskResponse _ _ c -> ["task_response", c]
    getAttributes = \case
        TerminalChat id _ ->
            [("session", numToText $ cast id)]
        TerminalTaskResponse _ id _ ->
            [("id", numToText $ cast id)]
        _ -> []
        where
            numToText :: Int -> Text
            numToText = TL.toStrict . TLB.toLazyText . TLB.decimal

getReplyingTimePhase ::
    ( ImmanentTime :> es )
    => Eff es (Maybe TimePhase)
getReplyingTimePhase =
    getLatestRetent >>= \case
        Just phase
            | Just (TerminalReply rp _) <- getImmanent @Terminal phase -> do
            pure $ Just rp
        _ -> pure Nothing

getReplyingTerminalContent ::
    ( ImmanentTime :> es )
    => Eff es (Maybe (Terminal, TimePhase))
getReplyingTerminalContent = do
    getReplyingTimePhase >>= \case
        Just phase
            | Just c <- getImmanent @Terminal phase -> do
            pure $ Just (c, phase)
        _ -> pure Nothing