module Heco.Data.InternalTimeStreamError where

import Control.Exception (Exception)

data InternalTimeStreamError
    = UnhandledInternalTimeStreamError String
    deriving (Eq, Show)

instance Exception InternalTimeStreamError