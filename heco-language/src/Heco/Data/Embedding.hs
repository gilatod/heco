module Heco.Data.Embedding where

import Data.Vector.Unboxing (Vector)
import Pattern.Cast (Cast(..))

newtype Embedding = Embedding (Vector Float)
    deriving (Eq, Show)

instance Cast Embedding (Vector Float) where
    cast (Embedding e) = e