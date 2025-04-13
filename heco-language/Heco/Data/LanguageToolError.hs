module Heco.Data.LanguageToolError where

import Data.Data (Typeable)
import Control.Exception (Exception)

data LanguageToolError
    = LanguageToolNotFoundError String
    deriving (Eq, Show, Typeable)

instance Exception LanguageToolError