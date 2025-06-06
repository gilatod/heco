module Heco.Data.Collection where

import Data.Text (Text)
import Data.String (IsString)
import Data.Hashable (Hashable)
import Pattern.Cast (Cast(..))

newtype CollectionName = CollectionName Text
    deriving (Show, Eq)
    deriving newtype (Hashable, IsString)

instance Cast CollectionName Text where
    cast (CollectionName t) = t