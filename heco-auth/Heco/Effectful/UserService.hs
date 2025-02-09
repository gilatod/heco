module Heco.Effectful.UserService where

import Heco.Data.Session (Session)

import Data.Text (Text)
import Data.UUID (UUID)
import Effectful (Effect)
import Effectful.TH (makeEffect)

data UserService :: Effect where
    SetNickname :: Session -> Text -> UserService m ()
    SetEmail :: UUID -> Text -> UserService m ()
    AddKnowledge :: UUID -> Text -> UserService m UUID
    RemoveKnowledge :: UUID -> UUID -> UserService m Bool
    ClearKnowledge :: UUID -> UserService m ()
    GetKnowledges :: UUID -> UserService m [(UUID, Text)]

makeEffect ''UserService