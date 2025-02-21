module Heco.Data.LanguageError where

import Data.Data (Typeable)
import Control.Exception (Exception)

data LanguageError =
    LanguageBackendError String
    deriving (Eq, Show, Typeable)

instance Exception LanguageError