module Heco.Data.Memory where

import Data.Time (UTCTime)
import Data.Text (Text)
import Heco.Data.Aeson (defaultAesonOps)
import Data.Aeson.TH (deriveJSON)

data Memory = Memory
    { topic :: [Text]
    , baseId :: Maybe Int
    , content :: Text
    , time :: Maybe UTCTime }
    deriving (Eq, Show)

deriveJSON defaultAesonOps ''Memory