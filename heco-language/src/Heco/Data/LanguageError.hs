module Heco.Data.LanguageError where

import Control.Exception (Exception)

data LanguageError
    = LanguageInputError String
    | LanguageBackendError String
    | UnhandledLanguageError String
    deriving (Eq, Show)

instance Exception LanguageError