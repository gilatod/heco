module Heco.Data.Role where

import Data.Aeson (ToJSON(..), FromJSON(..), Value(..))

data Role
    = System
    | User
    | Assistant
    | Tool
    deriving (Eq, Show, Enum)

instance ToJSON Role where
    toJSON System = String "system"
    toJSON User = String "user"
    toJSON Assistant = String "assistant"
    toJSON Tool = String "tool"

instance FromJSON Role where
    parseJSON "system" = pure System
    parseJSON "user" = pure User
    parseJSON "assistant" = pure Assistant
    parseJSON "tool" = pure Tool
    parseJSON _ = fail "Invalid Role value"