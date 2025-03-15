module Heco.Data.InternalTimeStreamError where

import Data.Data (Typeable)
import Control.Exception (Exception)

data InternalTimeStreamError
    = UnhandledInternalTimeStreamError String
    deriving (Eq, Show, Typeable)

instance Exception InternalTimeStreamError