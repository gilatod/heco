module Heco.Data.Message where

import Heco.Data.Role (Role)
import Heco.Data.Aeson (defaultAesonOps)

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson.TH (deriveJSON)
import Data.Aeson (Object)

data ToolCall = ToolCall
    { name :: Text
    , arguments :: Object }
    deriving (Eq, Show, Generic)

data Message = Message
    { role :: Role
    , toolCalls :: [ToolCall]
    , content :: Text }
    deriving (Eq, Show, Generic)

deriveJSON defaultAesonOps ''ToolCall
deriveJSON defaultAesonOps ''Message