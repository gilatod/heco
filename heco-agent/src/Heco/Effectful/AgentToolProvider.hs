module Heco.Effectful.AgentToolProvider where

import Heco.Data.FunctionSchema (FunctionSchema)

import Effectful (Effect)
import Effectful.TH (makeEffect)
import Effectful.Exception (SomeException)

import Data.Text (Text)
import Data.Aeson (Value)
import Data.HashMap.Strict (HashMap)

data AgentToolProvider :: Effect where
    GetAgentToolSchemas :: AgentToolProvider m [FunctionSchema]
    LookupAgentToolSchema :: Text -> AgentToolProvider m (Maybe FunctionSchema)
    InvokeAgentTool :: Text -> HashMap Text Value -> AgentToolProvider m (Either SomeException Value)

makeEffect ''AgentToolProvider