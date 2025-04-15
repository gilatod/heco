module Heco.Data.Immanant.Action where

import Heco.Data.TimePhase (ImmanantContent(..))

import Data.Text (Text)

data Action
    = StatementAction Text
    | ReasoningAction Text
    | QueryAction Text
    | WishAction Text
    | WillAction Text
    | ImaginativeAction Text
    deriving (Eq, Show)

instance ImmanantContent Action where
    encodeImmanantContent = \case
        StatementAction c -> ["say", c]
        ReasoningAction c -> ["think", c]
        QueryAction c -> ["query", c]
        WishAction c -> ["wish", c]
        WillAction c -> ["will", c]
        ImaginativeAction c -> ["imagine", c]