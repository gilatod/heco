module Heco.Data.Message where

import Heco.Data.Role (Role)

import Data.Text (Text)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data Message = Message
    { role :: Role
    , content :: Text }
    deriving (Eq, Show, Generic)

instance FromJSON Message
instance ToJSON Message