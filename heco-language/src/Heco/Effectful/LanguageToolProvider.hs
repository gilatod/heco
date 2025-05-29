module Heco.Effectful.LanguageToolProvider where

import Heco.Data.FunctionSchema (FunctionSchema)

import Effectful (Effect)
import Effectful.TH (makeEffect)
import Effectful.Exception (SomeException)

import Data.Text (Text)
import Data.Aeson (Value)
import Data.HashMap.Strict (HashMap)

data LanguageToolProvider :: Effect where
    GetLanguageToolSchemas :: LanguageToolProvider m [FunctionSchema]
    LookupLanguageToolSchema :: Text -> LanguageToolProvider m (Maybe FunctionSchema)
    InvokeLanguageTool :: Text -> HashMap Text Value -> LanguageToolProvider m (Either SomeException Value)

makeEffect ''LanguageToolProvider