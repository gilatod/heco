module Heco.Data.Message where

import Heco.Data.Role (Role)
import Heco.Data.Aeson (defaultAesonOps)

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson.TH (deriveJSON)

data Message = Message
    { role :: Role
    , content :: Text }
    deriving (Eq, Show, Generic)

deriveJSON defaultAesonOps ''Message