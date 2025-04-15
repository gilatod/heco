module Heco.Data.LanguageToolError where

import Control.Exception (Exception)

data LanguageToolError
    = LanguageToolArgumentNotFound String
    | LanguageToolArgumentInvalid String
    | LanguageToolNotFoundError String
    deriving (Eq, Show)

instance Exception LanguageToolError