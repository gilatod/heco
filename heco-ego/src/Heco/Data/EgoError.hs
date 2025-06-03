module Heco.Data.EgoError where

import Control.Exception (Exception)

data EgoError
    = EgoMemoryEmbeddingError String
    | EgoInvalidReplyError String
    deriving (Eq, Show)

instance Exception EgoError