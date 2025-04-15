module Heco.Data.EgoError where

import Control.Exception (Exception)

data EgoError
    = EgoMemoryEmbeddingError String
    | EgoInvalidNoemaError String
    | UnhandledEgoError String
    deriving (Eq, Show)

instance Exception EgoError