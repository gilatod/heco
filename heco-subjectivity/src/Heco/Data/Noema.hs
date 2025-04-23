module Heco.Data.Noema where

import Heco.Data.TimePhase (ImmanantContent(..))

import Data.Text (Text)
import Data.Hashable (Hashable)
import Data.Aeson (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Pattern.Cast (Cast(..))

newtype NoemaId = NoemaId Int
    deriving (Show, Eq, Ord, Enum, Bounded)
    deriving newtype Hashable

deriveJSON defaultOptions ''NoemaId

instance Cast NoemaId Int where
    cast (NoemaId id) = id

data Noema = Noema
    { id :: NoemaId
    , content :: [Text] }
    deriving (Eq, Show)

instance ImmanantContent Noema where
    encodeImmanantContent n = "object":n.content