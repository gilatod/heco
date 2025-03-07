module Heco.Data.Embedding where

import Data.Vector.Unboxing (Vector)

newtype Embedding = Embedding (Vector Float)
    deriving (Eq, Show)