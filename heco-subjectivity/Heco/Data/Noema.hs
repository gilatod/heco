module Heco.Data.Noema where

import Data.Text (Text)
import Data.Hashable (Hashable)
import Data.Aeson (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Data.String (IsString)
import Pattern.Cast (Cast(..))

newtype NoemaId = NoemaId Int
    deriving (Show, Eq, Hashable, Ord, Enum, Bounded)

deriveJSON defaultOptions ''NoemaId

instance Cast NoemaId Int where
    cast (NoemaId id) = id

newtype NoemaCategory = NoemaCategory Text
    deriving (Show, Eq, Hashable, IsString)

instance Cast NoemaCategory Text where
    cast (NoemaCategory t) = t

data Noema = Noema
    { id :: NoemaId
    , category :: NoemaCategory
    , content :: Text }
    deriving (Eq, Show)