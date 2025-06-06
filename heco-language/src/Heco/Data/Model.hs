module Heco.Data.Model where

import Data.Text (Text)
import Data.String (IsString)
import Data.Hashable (Hashable)
import Pattern.Cast (Cast(..))

newtype ModelName = ModelName Text
    deriving (Show, Eq)
    deriving newtype (Hashable, IsString)

instance Cast ModelName Text where
    cast (ModelName t) = t