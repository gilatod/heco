module Heco.Data.Model where

import Data.Text (Text)
import Data.String (IsString)
import Data.Hashable (Hashable)
import Pattern.Cast (Cast(..))

newtype ModelName = ModelName Text
    deriving (Show, Eq, Hashable, IsString)

instance Cast ModelName Text where
    cast :: ModelName -> Text
    cast (ModelName t) = t