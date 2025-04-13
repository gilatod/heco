module Heco.Effectful.LanguageToolProvider where

import Heco.Data.FunctionSchema (FunctionSchema)

import Effectful (Effect)

import Data.Text (Text)
import Data.Aeson (Value)
import Data.HashMap.Strict (HashMap)
import Effectful.TH (makeEffect)

data LanguageToolProvider :: Effect where
    GetLanguageTools :: LanguageToolProvider m [FunctionSchema]
    InvokeLanguageTool :: Text -> HashMap Text Value -> LanguageToolProvider m Value

makeEffect ''LanguageToolProvider