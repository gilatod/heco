module Heco.Data.AgentToolError where

import Control.Exception (Exception)

data AgentToolError
    = AgentToolArgumentNotFound String
    | AgentToolArgumentInvalid String
    | AgentToolNotFoundError String
    deriving (Eq, Show)

instance Exception AgentToolError