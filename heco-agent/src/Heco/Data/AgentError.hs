module Heco.Data.AgentError where

import Control.Exception (Exception)

data AgentError
    = AgentMemoryEmbeddingError String
    | AgentInvalidReplyError String
    deriving (Eq, Show)

instance Exception AgentError