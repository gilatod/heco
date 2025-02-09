module Heco.Data.User where

import Data.Text (Text)
import Heco.Data.AuthGroup (GroupName)
import Data.Hashable (Hashable)
import Data.String (IsString)

newtype Username = Username Text
    deriving (Eq, Show, Hashable, IsString)

data User = User
    { username :: Username
    , nickname :: Text
    , email :: Text
    , groups :: [GroupName] }
    deriving (Show)