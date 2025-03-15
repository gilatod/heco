module Heco.Data.EgoError where

import Data.Data (Typeable)
import Control.Exception (Exception)

data EgoError
    = EgoMemoryEmbeddingError String
    | EgoInvalidNoemaError String
    | UnhandledEgoError String
    deriving (Eq, Show, Typeable)

instance Exception EgoError